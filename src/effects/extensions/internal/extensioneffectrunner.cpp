/*
 * Audacity: A Digital Audio Editor
 */
#include "extensioneffectrunner.h"

#include <algorithm>
#include <cmath>
#include <limits>
#include <memory>
#include <utility>
#include <vector>

#include <QTimer>

#include "au3-effects/EffectOutputTracks.h"
#include "au3-label-track/LabelTrack.h"
#include "au3-project/Project.h"
#include "au3-strings/Internat.h"
#include "au3-track/Track.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3wrap/au3projectcontext.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "context/iglobalcontext.h"
#include "trackedit/api/projectedit.h"

#include "extensioneffect.h"

namespace au::effects::extensions {
ExtensionEffectTask::ExtensionEffectTask(std::function<bool(double, std::string)> progress)
    : m_progress(std::move(progress))
{
}

bool ExtensionEffectTask::cancelled() const
{
    return m_cancelled;
}

bool ExtensionEffectTask::report(double fraction, const QString& message)
{
    if (!std::isfinite(fraction) || fraction < 0.0 || fraction > 1.0 || cancelled()) {
        return false;
    }
    m_fraction = fraction;
    m_message = message.toStdString();
    refresh();
    return !cancelled();
}

void ExtensionEffectTask::refresh()
{
    if (!cancelled()) {
        m_cancelled = m_progress(m_fraction, m_message);
    }
}

void ExtensionPromiseObserver::resolve(const QJSValue& value)
{
    QTimer::singleShot(0, this, [this, value] {
        emit resolved(value);
    });
}

void ExtensionPromiseObserver::reject(const QJSValue& reason)
{
    QString error = reason.toString();
    const QJSValue stack = reason.property(QStringLiteral("stack"));
    if (stack.isString()) {
        error += QLatin1Char('\n') + stack.toString();
    }
    QTimer::singleShot(0, this, [this, error = std::move(error)] {
        emit rejected(error);
    });
}

namespace {
std::vector<trackedit::api::ProjectAudioTrack> selectedAudio(EffectOutputTracks& renderedInputs)
{
    std::vector<trackedit::api::ProjectAudioTrack> result;
    for (auto* track : renderedInputs.Get().Selected<WaveTrack>()) {
        if (track->NChannels() != 1 && track->NChannels() != 2) {
            continue;
        }
        const auto* original = renderedInputs.GetMatchingInput(*track);
        if (!original) {
            continue;
        }
        const auto* originalWave = static_cast<const WaveTrack*>(original);
        trackedit::api::ProjectAudioTrack input {
            track->SharedPointer<WaveTrack>(),
            std::const_pointer_cast<WaveTrack>(originalWave->SharedPointer<const WaveTrack>()),
            au3::wxToStdString(track->GetName()),
            true,
            {},
        };
        for (const auto& clip : originalWave->Intervals()) {
            input.clips.push_back({
                        clip->GetId(), clip->GetPlayStartTime(), clip->GetPlayEndTime(), au3::wxToStdString(clip->GetName()), false
                    });
        }
        result.push_back(std::move(input));
    }
    return result;
}

std::vector<trackedit::api::ProjectLabelTrack> selectedLabels(ExtensionEffect& effect)
{
    std::vector<trackedit::api::ProjectLabelTrack> result;
    constexpr double infinity = std::numeric_limits<double>::infinity();
    for (auto* track : effect.mTracks->Selected<LabelTrack>()) {
        result.push_back(trackedit::api::snapshotLabelTrack(*track, -infinity, infinity));
    }
    return result;
}
} // namespace

ExtensionEffectRun::ExtensionEffectRun(ExtensionEffect& effect, ExtensionEffectInstance& instance, EffectSettings& settings,
                                       Completion completion)
    : m_effect(effect), m_instance(instance), m_settings(settings), m_completion(std::move(completion)),
    m_task([&effect](double fraction, std::string message) {
    return effect.TotalProgress(fraction, Verbatim(message));
}) {
    connect(&m_observer, &ExtensionPromiseObserver::resolved, this, [this](const QJSValue& result) {
        m_promisePending = false;
        finish(result);
    });
    connect(&m_observer, &ExtensionPromiseObserver::rejected, this, [this](const QString& error) {
        m_promisePending = false;
        fail(error.toStdString());
    });
}

ExtensionEffectRun::~ExtensionEffectRun() = default;

void ExtensionEffectRun::start()
{
    if (!m_instance.applySettings(m_settings)) {
        fail("Invalid effect settings");
        return;
    }
    const muse::Ret validation = m_instance.validate();
    if (!validation) {
        fail(validation.toString());
        return;
    }
    m_instance.writeCurrentSettings(m_settings);

    const double selectionStart = std::min(m_effect.mT0, m_effect.mT1);
    const double selectionEnd = std::max(m_effect.mT0, m_effect.mT1);
    const std::optional<EffectOutputTracks::TimeInterval> interval{ { selectionStart, selectionEnd } };
    m_renderedInputs = std::make_unique<EffectOutputTracks>(*m_effect.mTracks, EffectTypeProcess, interval);
    auto audio = selectedAudio(*m_renderedInputs);
    auto labels = selectedLabels(m_effect);
    const bool requiresInput = m_effect.GetType() != EffectTypeGenerate && m_effect.GetType() != EffectTypeTool;
    if (requiresInput && audio.empty() && labels.empty()) {
        fail("No supported input tracks are selected");
        return;
    }

    std::shared_ptr<WaveTrack> generatorDestination;
    if (m_effect.GetType() == EffectTypeGenerate && m_effect.mNumTracks == 0) {
        const auto selected = m_effect.mTracks->Selected<WaveTrack>();
        if (auto iterator = selected.begin(); iterator != selected.end()) {
            generatorDestination = (*iterator)->SharedPointer<WaveTrack>();
        }
    }

    trackedit::api::ProjectEditWorkspace workspace;
    auto* project = m_effect.mTracks->GetOwner();
    if (!project) {
        fail("The effect track list has no project");
        return;
    }
    workspace.project = project->shared_from_this();
    workspace.tracks = m_effect.mTracks.get();
    workspace.factory = m_effect.mFactory;
    workspace.audioTracks = std::move(audio);
    workspace.labelTracks = std::move(labels);
    workspace.preferredAudioDestination = std::move(generatorDestination);
    workspace.selectionStart = selectionStart;
    workspace.selectionEnd = selectionEnd;
    const auto projectContext = au3::projectIocContext(*project);
    const auto appContext = projectContext ? muse::modularity::ioc(projectContext)->resolve<au::context::IGlobalContext>(
        "effects_extensions") : nullptr;
    if (appContext) {
        workspace.projectAvailable = [appContext, project] {
            const auto active = appContext->currentProject();
            return active && reinterpret_cast<AudacityProject*>(active->au3ProjectPtr()) == project;
        };
    }
    m_projectSession = std::make_unique<trackedit::api::ProjectEditSession>(std::move(workspace));

    auto result = m_instance.m_runtime->process(m_instance.currentSettings(), *m_projectSession, &m_task);
    if (!result.ret) {
        fail(result.ret.toString());
        return;
    }

    auto observing = m_instance.m_runtime->observePromise(result.val, &m_observer);
    if (!observing.ret) {
        fail(observing.ret.toString());
        return;
    }

    if (!observing.val) {
        finish(result.val);
        return;
    }
    m_promise = result.val;
    m_promisePending = true;
}

void ExtensionEffectRun::finish(const QJSValue& result)
{
    if (result.isBool() && !result.toBool()) {
        fail("effect returned false");
        return;
    }
    if (m_task.cancelled()) {
        fail("The extension operation was cancelled");
        return;
    }

    std::string commitError;
    if (!m_projectSession->finish(commitError)) {
        fail(std::move(commitError));
        return;
    }
    m_instance.setLastError({});
    complete(true);
}

void ExtensionEffectRun::fail(std::string error)
{
    m_instance.setLastError(std::move(error));
    complete(false);
}

void ExtensionEffectRun::complete(bool success)
{
    if (m_finished) {
        return;
    }
    m_finished = true;
    if (m_promisePending) {
        m_instance.orphanRuntime(m_promise);
    }
    auto completion = std::move(m_completion);
    completion(success);
}

void ExtensionEffectRun::updateProgress()
{
    if (m_finished || m_updatingProgress) {
        return;
    }
    m_updatingProgress = true;
    m_task.refresh();
    if (!m_finished && m_instance.m_runtime->hasProgressCallback()) {
        const muse::Ret ret = m_instance.m_runtime->updateProgress(*m_projectSession, &m_task);
        if (!ret) {
            fail(ret.toString());
        }
    }
    m_updatingProgress = false;
}

bool ExtensionEffectRun::cancelled() const
{
    return m_task.cancelled();
}

void ExtensionEffectRun::abort(std::string error)
{
    fail(std::move(error));
}
} // namespace au::effects::extensions
