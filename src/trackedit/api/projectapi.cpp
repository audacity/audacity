/*
 * Audacity: A Digital Audio Editor
 */
#include "projectapi.h"

#include <algorithm>
#include <limits>

#include <QJSEngine>

#include "au3-project-rate/ProjectRate.h"
#include "au3-project/Project.h"
#include "log.h"

#include "projectedit.h"

using namespace au::trackedit;
using namespace au::trackedit::api;

namespace {
constexpr double INVALID_TIME = std::numeric_limits<double>::quiet_NaN();
}

ProjectApi::ProjectApi(muse::api::IApiEngine* e)
    : ApiObject(e)
{
}

AudacityProject* ProjectApi::project() const
{
    const auto current = globalContext() ? globalContext()->currentProject() : nullptr;
    return current ? reinterpret_cast<AudacityProject*>(current->au3ProjectPtr()) : nullptr;
}

QObject* ProjectApi::selection()
{
    auto* current = project();
    if (!current || !selectionController()) {
        return nullptr;
    }
    auto* result = makeCurrentProjectSelection(
        current, selectionController()->dataSelectedStartTime(), selectionController()->dataSelectedEndTime(),
        selectionController()->selectedClips(), nullptr);
    QJSEngine::setObjectOwnership(result, QJSEngine::JavaScriptOwnership);
    return result;
}

ITrackeditProjectPtr ProjectApi::trackeditProject() const
{
    return globalContext() ? globalContext()->currentTrackeditProject() : nullptr;
}

Clips ProjectApi::sortedClips(int trackIndex) const
{
    auto prj = trackeditProject();
    if (!prj) {
        return {};
    }
    auto ids = prj->trackIdList();
    if (trackIndex < 0 || trackIndex >= static_cast<int>(ids.size())) {
        LOGE() << "trackIndex out of range: " << trackIndex << " (have " << ids.size() << " tracks)";
        return {};
    }
    auto clipList = prj->clipList(ids[trackIndex]);
    Clips clips(clipList.begin(), clipList.end());
    std::sort(clips.begin(), clips.end(), [](const Clip& a, const Clip& b) {
        return a.startTime < b.startTime;
    });
    return clips;
}

int ProjectApi::trackCount() const
{
    auto prj = trackeditProject();
    return prj ? static_cast<int>(prj->trackIdList().size()) : 0;
}

int ProjectApi::clipCount(int trackIndex) const
{
    return static_cast<int>(sortedClips(trackIndex).size());
}

QJSValue ProjectApi::clipsOnTrack(int trackIndex) const
{
    const auto clips = sortedClips(trackIndex);
    QJSValue arr = engine()->newArray(clips.size());
    for (size_t i = 0; i < clips.size(); ++i) {
        QJSValue obj = engine()->newObject();
        obj.setProperty("start", static_cast<double>(clips[i].startTime));
        obj.setProperty("end", static_cast<double>(clips[i].endTime));
        arr.setProperty(static_cast<quint32>(i), obj);
    }
    return arr;
}

double ProjectApi::totalTime() const
{
    auto prj = trackeditProject();
    if (!prj) {
        return INVALID_TIME;
    }
    return static_cast<double>(prj->totalTime());
}

double ProjectApi::defaultSampleRate() const
{
    const auto* current = project();
    return current ? ProjectRate::Get(*current).GetRate() : INVALID_TIME;
}

QObject* ProjectApi::beginEdit(const QString& name, QObject* session)
{
    if (projectApiScopeActive()) {
        auto* editSession = dynamic_cast<ProjectEditSession*>(session);
        if (!editSession || editSession != currentProjectEditSession()) {
            throwProjectError(this, QStringLiteral("Project edits are not available in this callback"));
            return nullptr;
        }
        return editSession->beginEdit(this);
    }

    if (session) {
        throwProjectError(this, QStringLiteral("The project edit session is no longer active"));
        return nullptr;
    }

    auto* current = project();
    if (!current || !selectionController()) {
        throwProjectError(this, QStringLiteral("No project is active"));
        return nullptr;
    }
    if (name.isEmpty()) {
        throwProjectError(this, QStringLiteral("An undo description is required"));
        return nullptr;
    }
    if (recordController() && recordController()->isRecording()) {
        throwProjectError(this, QStringLiteral("Project edits are not available while recording"));
        return nullptr;
    }
    if (playbackController()) {
        playbackController()->stop();
    }
    const auto appContext = globalContext();
    const auto selection = selectionController();
    const auto target = appContext->currentProject();
    return beginRootProjectEdit(
        current, selectionController()->dataSelectedStartTime(), selectionController()->dataSelectedEndTime(),
        selectionController()->selectedClips(), name, projectHistory(),
        [appContext, target, current] {
        const auto active = appContext->currentProject();
        return active == target && active && reinterpret_cast<AudacityProject*>(active->au3ProjectPtr()) == current;
    },
        [selection](double start, double end, const std::vector<int64_t>& tracks) {
        selection->resetSelectedClips();
        selection->resetSelectedLabels();
        selection->setSelectedTracks(tracks, true);
        selection->setDataSelectedStartTime(start, true);
        selection->setDataSelectedEndTime(end, true);
    },
        this);
}
