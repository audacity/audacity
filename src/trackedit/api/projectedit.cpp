/*
 * Audacity: A Digital Audio Editor
 */
#include "projectedit.h"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <limits>
#include <map>
#include <set>
#include <thread>
#include <utility>

#include <QJSEngine>

#include "au3-label-track/LabelTrack.h"
#include "au3-project/Project.h"
#include "au3-time-frequency-selection/SelectedRegion.h"
#include "au3-track/TimeWarper.h"
#include "au3-track/Track.h"
#include "au3-transactions/TransactionScope.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "framework/global/runtime.h"
#include "log.h"

#include "internal/projecteditstate.h"

namespace au::trackedit::api::detail {
namespace {
std::set<AudacityProject*> activeEdits;
}

EditState::~EditState()
{
    release();
}

bool EditState::acquire()
{
    if (std::this_thread::get_id() != muse::runtime::mainThreadId()) {
        error = "Project edits must run on the main thread";
        return false;
    }
    if (!available()) {
        error = "No project is active";
        return false;
    }
    if (activeEdits.find(project.get()) != activeEdits.end()) {
        error = "Another project edit is already active";
        return false;
    }
    activeEdits.insert(project.get());
    lockHeld = true;
    return true;
}

void EditState::release()
{
    if (lockHeld) {
        assert(std::this_thread::get_id() == muse::runtime::mainThreadId());
        activeEdits.erase(project.get());
        lockHeld = false;
    }
}

bool EditState::usable() const
{
    return active && !committed && available();
}

bool EditState::writable() const
{
    return usable() && editStarted;
}

bool EditState::available() const
{
    return project && (!projectAvailable || projectAvailable());
}

bool EditState::hasSelectedClips() const
{
    return std::any_of(audioTracks.begin(), audioTracks.end(), [](const ProjectAudioTrack& track) {
        return std::any_of(track.clips.begin(), track.clips.end(), [](const ProjectAudioClip& clip) {
            return clip.selected;
        });
    });
}

bool EditState::hasSelectedLabels() const
{
    return std::any_of(labelTracks.begin(), labelTracks.end(), [](const ProjectLabelTrack& track) {
        return std::any_of(track.labels.begin(), track.labels.end(), [](const ProjectLabel& label) {
            return label.selected;
        });
    });
}

bool validTimes(double start, double end)
{
    return std::isfinite(start) && std::isfinite(end) && start >= 0.0 && end >= start;
}

bool isCurrentTrack(const EditState& state, const std::shared_ptr<Track>& track)
{
    return state.tracks && track && state.tracks->FindById(track->GetId()) == track.get();
}

bool prepareChanges(EditState& state, PreparedChanges& prepared, std::string& error)
{
    prepared.audioReplacements = TrackList::Temporary(state.tracks->GetOwner());
    for (const auto& replacement : state.replacements) {
        if (!isCurrentTrack(state, replacement.destination)) {
            error = "An audio track changed while the edit was open";
            return false;
        }
        if (replacement.wholeTrack) {
            replacement.audio->SetName(replacement.name.empty() ? replacement.destination->GetName() : au3::wxFromStdString(
                                           replacement.name));
            replacement.audio->MoveTo(replacement.start);
            prepared.audioReplacements->Add(replacement.audio);
            prepared.audioDestinations.push_back(replacement.destination);
            continue;
        }

        const auto isClipBoundary = [&](double time) {
            const auto sample = replacement.destination->TimeToLongSamples(time);
            const auto intervals = replacement.destination->Intervals();
            return std::any_of(intervals.begin(), intervals.end(), [&](const auto& clip) {
                return clip->GetPlayStartSample() == sample || clip->GetPlayEndSample() == sample;
            });
        };
        const bool merge = !(isClipBoundary(replacement.start) && isClipBoundary(replacement.end));
        auto duplicate = std::static_pointer_cast<WaveTrack>(replacement.destination->Duplicate(Track::DuplicateOptions {}.Backup()));
        const double duration = replacement.audio->GetEndTime() - replacement.audio->GetStartTime();
        PasteTimeWarper warper{
            replacement.end,
            replacement.start + duration,
        };
        duplicate->ClearAndPaste(replacement.start, replacement.end, *replacement.audio, true, merge, &warper);
        prepared.audioReplacements->Add(duplicate);
        prepared.audioDestinations.push_back(replacement.destination);
    }

    for (size_t trackIndex = 0; trackIndex < state.audioTracks.size(); ++trackIndex) {
        const bool changed = std::any_of(state.clipChanges.begin(), state.clipChanges.end(), [&](const ClipChange& change) {
            return change.trackIndex == trackIndex;
        });
        if (!changed) {
            continue;
        }

        const auto& input = state.audioTracks[trackIndex];
        if (!isCurrentTrack(state, input.destination)) {
            error = "An audio track changed while the edit was open";
            return false;
        }
        if (std::any_of(state.replacements.begin(), state.replacements.end(), [&](const Replacement& replacement) {
            return replacement.destination == input.destination;
        })) {
            error = "An audio track cannot change audio and clip titles in one edit";
            return false;
        }

        auto duplicate = std::static_pointer_cast<WaveTrack>(input.destination->Duplicate(Track::DuplicateOptions {}.Backup()));
        std::map<int64_t, std::shared_ptr<WaveClip> > duplicateClips;
        auto originals = input.destination->Intervals();
        auto copies = duplicate->Intervals();
        auto original = originals.begin();
        auto copy = copies.begin();
        while (original != originals.end() && copy != copies.end()) {
            duplicateClips.emplace((*original)->GetId(), *copy);
            ++original;
            ++copy;
        }
        for (const auto& change : state.clipChanges) {
            if (change.trackIndex != trackIndex) {
                continue;
            }
            const auto found = duplicateClips.find(change.nativeId);
            if (found == duplicateClips.end()) {
                error = "A selected audio clip no longer exists";
                return false;
            }
            found->second->SetName(au3::wxFromStdString(change.title));
        }
        prepared.audioReplacements->Add(duplicate);
        prepared.audioDestinations.push_back(input.destination);
    }

    prepared.labelReplacements = TrackList::Temporary(state.tracks->GetOwner());
    for (size_t trackIndex = 0; trackIndex < state.labelTracks.size(); ++trackIndex) {
        const bool changed = std::any_of(state.labelChanges.begin(), state.labelChanges.end(), [&](const LabelChange& change) {
            return change.trackIndex == trackIndex;
        });
        if (!changed) {
            continue;
        }

        const auto& input = state.labelTracks[trackIndex];
        if (!isCurrentTrack(state, input.track)) {
            error = "A label track changed while the edit was open";
            return false;
        }
        auto duplicate = std::static_pointer_cast<LabelTrack>(input.track->Duplicate(Track::DuplicateOptions {}.Backup()));
        std::map<int64_t, int64_t> duplicateIds;
        for (int index = 0; index < input.track->GetNumLabels(); ++index) {
            const auto* original = input.track->GetLabel(index);
            const auto* copy = duplicate->GetLabel(index);
            if (original && copy) {
                duplicateIds.emplace(original->GetId(), copy->GetId());
            }
        }
        for (const auto& change : state.labelChanges) {
            if (change.trackIndex != trackIndex) {
                continue;
            }
            if (!change.nativeId) {
                duplicate->AddLabel(SelectedRegion(change.start, change.end), au3::wxFromStdString(change.text));
                continue;
            }

            const auto mapped = duplicateIds.find(*change.nativeId);
            if (mapped == duplicateIds.end()) {
                error = "A selected label no longer exists";
                return false;
            }
            if (change.remove) {
                duplicate->DeleteLabelById(mapped->second);
                continue;
            }
            const int duplicateIndex = duplicate->GetLabelIndex(mapped->second);
            if (duplicateIndex < 0) {
                error = "A selected label no longer exists";
                return false;
            }
            auto updated = *duplicate->GetLabel(duplicateIndex);
            updated.selectedRegion.setTimes(change.start, change.end);
            updated.title = au3::wxFromStdString(change.text);
            duplicate->SetLabel(static_cast<size_t>(duplicateIndex), updated);
        }
        prepared.labelReplacements->Add(duplicate);
        prepared.labelDestinations.push_back(input.track);
    }

    prepared.additions = TrackList::Temporary(state.tracks->GetOwner());
    for (const auto& added : state.addedAudio) {
        added.audio->SetName(au3::wxFromStdString(added.name));
        added.audio->MoveTo(added.start);
        prepared.additions->Add(added.audio);
    }
    for (const auto& added : state.addedLabelTracks) {
        auto track = LabelTrack::CreatePtr(*state.tracks);
        track->SetName(au3::wxFromStdString(added.name));
        for (const auto& label : added.labels) {
            track->AddLabel(SelectedRegion(label.start, label.end), au3::wxFromStdString(label.text));
        }
        prepared.additions->Add(track);
    }
    return true;
}

bool applyChanges(EditState& state, std::string& error, AppliedChanges* applied)
{
    if (!state.available() || !state.tracks || !state.factory) {
        error = "The project edit target is unavailable";
        return false;
    }

    state.selectedTrackIds.clear();
    if (state.selectionChanged) {
        for (const auto& track : state.audioTracks) {
            if (track.selected) {
                if (!isCurrentTrack(state, track.destination)) {
                    error = "A selected audio track changed while the edit was open";
                    return false;
                }
                state.selectedTrackIds.push_back(track.destination->GetId());
            }
        }
        for (const auto& track : state.labelTracks) {
            if (track.selected) {
                if (!isCurrentTrack(state, track.track)) {
                    error = "A selected label track changed while the edit was open";
                    return false;
                }
                state.selectedTrackIds.push_back(track.track->GetId());
            }
        }
    }

    if (!state.prepared) {
        state.prepared = std::make_unique<PreparedChanges>();
    }
    if (!state.committed && !prepareChanges(state, *state.prepared, error)) {
        return false;
    }

    for (const auto& destination : state.prepared->audioDestinations) {
        if (!isCurrentTrack(state, destination)) {
            error = "An audio track changed while the edit was open";
            return false;
        }
    }
    for (const auto& destination : state.prepared->labelDestinations) {
        if (!isCurrentTrack(state, destination)) {
            error = "A label track changed while the edit was open";
            return false;
        }
    }

    if (applied) {
        applied->replacements.clear();
        applied->replacements.reserve(state.prepared->audioDestinations.size() + state.prepared->labelDestinations.size());
        applied->additions.clear();
        for (Track* addition : *state.prepared->additions) {
            applied->additions.push_back(addition->shared_from_this());
        }
    }
    for (const auto& destination : state.prepared->audioDestinations) {
        const auto replacement = (*state.prepared->audioReplacements->begin())->shared_from_this();
        auto original = state.tracks->ReplaceOne(*destination, std::move(*state.prepared->audioReplacements));
        if (applied) {
            applied->replacements.push_back({
                    std::move(original),
                    replacement,
                });
        }
    }
    for (const auto& destination : state.prepared->labelDestinations) {
        const auto replacement = (*state.prepared->labelReplacements->begin())->shared_from_this();
        auto original = state.tracks->ReplaceOne(*destination, std::move(*state.prepared->labelReplacements));
        if (applied) {
            applied->replacements.push_back({
                    std::move(original),
                    replacement,
                });
        }
    }
    state.tracks->Append(std::move(*state.prepared->additions));
    return true;
}

bool rollbackChanges(EditState& state, AppliedChanges& applied, std::string& error)
{
    try {
        for (auto iterator = applied.additions.rbegin(); iterator != applied.additions.rend(); ++iterator) {
            if (isCurrentTrack(state, *iterator)) {
                state.tracks->Remove(**iterator);
            }
        }
        for (auto iterator = applied.replacements.rbegin(); iterator != applied.replacements.rend(); ++iterator) {
            if (!isCurrentTrack(state, iterator->replacement)) {
                error = "Could not restore a replaced project track";
                return false;
            }
            auto original = TrackList::Temporary(state.tracks->GetOwner());
            original->Add(iterator->original);
            state.tracks->ReplaceOne(*iterator->replacement, std::move(*original));
        }
        applied = {};
        return true;
    } catch (const std::exception& exception) {
        error = exception.what();
    } catch (...) {
        error = "Could not restore project tracks";
    }
    return false;
}

std::shared_ptr<EditState> currentState(
    AudacityProject* project, double start, double end, const ClipKeyList& selectedClips, bool allTracks)
{
    auto state = std::make_shared<EditState>();
    if (project) {
        state->project = project->shared_from_this();
    }
    state->tracks = project ? &TrackList::Get(*project) : nullptr;
    state->factory = project ? &WaveTrackFactory::Get(*project) : nullptr;
    state->selectionStart = std::min(start, end);
    state->selectionEnd = std::max(start, end);
    if (!state->tracks) {
        return state;
    }

    state->sourceTracks = TrackList::Temporary(state->project.get());
    const auto addAudioTrack = [&](WaveTrack* track) {
        if (track->NChannels() != 1 && track->NChannels() != 2) {
            return;
        }
        auto destination = track->SharedPointer<WaveTrack>();
        auto source = std::static_pointer_cast<WaveTrack>(track->Duplicate(Track::DuplicateOptions {}.Backup()));
        state->sourceTracks->Add(source);
        ProjectAudioTrack input {
            std::move(source),
            std::move(destination),
            au3::wxToStdString(track->GetName()),
            !allTracks || track->GetSelected(),
            {},
        };
        for (const auto& clip : track->Intervals()) {
            const bool selected = std::any_of(selectedClips.begin(), selectedClips.end(), [&](const ClipKey& key) {
                return key.trackId == track->GetId() && key.itemId == clip->GetId();
            });
            input.clips.push_back({
                    clip->GetId(), clip->GetPlayStartTime(), clip->GetPlayEndTime(), au3::wxToStdString(clip->GetName()), selected
                });
        }
        state->audioTracks.push_back(std::move(input));
    };
    if (allTracks) {
        for (auto* track : state->tracks->Any<WaveTrack>()) {
            addAudioTrack(track);
        }
    } else {
        for (auto* track : state->tracks->Selected<WaveTrack>()) {
            addAudioTrack(track);
        }
    }

    constexpr double infinity = std::numeric_limits<double>::infinity();
    const auto addLabelTrack = [&](LabelTrack* track) {
        auto selected = snapshotLabelTrack(*track, -infinity, infinity);
        selected.selected = !allTracks || track->GetSelected();
        state->labelTracks.push_back(std::move(selected));
    };
    if (allTracks) {
        for (auto* track : state->tracks->Any<LabelTrack>()) {
            addLabelTrack(track);
        }
    } else {
        for (auto* track : state->tracks->Selected<LabelTrack>()) {
            addLabelTrack(track);
        }
    }
    return state;
}
} // namespace au::trackedit::api::detail

namespace au::trackedit::api {
thread_local bool apiScopeActive = false;
thread_local ProjectEditSession* editSession = nullptr;

ProjectEditSession::ProjectEditSession(ProjectEditWorkspace workspace, QObject* parent)
    : QObject(parent)
{
    auto state = std::make_shared<detail::EditState>();
    state->project = workspace.project;
    state->tracks = workspace.tracks;
    state->factory = workspace.factory;
    state->audioTracks = std::move(workspace.audioTracks);
    state->labelTracks = std::move(workspace.labelTracks);
    state->preferredAudioDestination = std::move(workspace.preferredAudioDestination);
    state->projectAvailable = std::move(workspace.projectAvailable);
    state->selectionStart = std::min(workspace.selectionStart, workspace.selectionEnd);
    state->selectionEnd = std::max(workspace.selectionStart, workspace.selectionEnd);
    state->deferredCommit = true;
    if (!state->tracks || !state->factory || !state->acquire()) {
        state->active = false;
    }
    m_state = std::move(state);
}

ProjectEditSession::~ProjectEditSession()
{
    invalidate();
}

bool ProjectEditSession::finish(std::string& error)
{
    auto& state = *m_state;
    if (!state.active) {
        error = state.error.empty() ? "The project edit is no longer active" : state.error;
        return false;
    }
    if (!state.editStarted) {
        state.active = false;
        state.release();
        return true;
    }
    if (!state.committed) {
        error = "The extension did not commit its project edit";
        invalidate();
        return false;
    }
    detail::AppliedChanges applied;
    const auto rollback = [&] {
        std::string rollbackError;
        if (!detail::rollbackChanges(state, applied, rollbackError)) {
            LOGE() << "failed to restore project edit: " << rollbackError;
        }
    };
    try {
        if (!detail::applyChanges(state, error, &applied)) {
            rollback();
            invalidate();
            return false;
        }
        if (state.selectionChanged && state.setSelection) {
            state.setSelection(state.selectionStart, state.selectionEnd, state.selectedTrackIds);
        }
        state.active = false;
        state.release();
        return true;
    } catch (const std::exception& exception) {
        error = exception.what();
    } catch (...) {
        error = "Could not commit project changes";
    }
    rollback();
    invalidate();
    return false;
}

void ProjectEditSession::invalidate()
{
    if (m_state) {
        m_state->active = false;
        m_state->release();
    }
}

ProjectApiScope::ProjectApiScope(ProjectEditSession* session)
    : m_previousSession(editSession), m_previousActive(apiScopeActive)
{
    apiScopeActive = true;
    editSession = session;
}

ProjectApiScope::~ProjectApiScope()
{
    apiScopeActive = m_previousActive;
    editSession = m_previousSession;
}

bool projectApiScopeActive()
{
    return apiScopeActive;
}

ProjectEditSession* currentProjectEditSession()
{
    return editSession;
}

ProjectLabelTrack snapshotLabelTrack(LabelTrack& track, double start, double end)
{
    ProjectLabelTrack result;
    result.track = track.SharedPointer<LabelTrack>();
    result.name = au3::wxToStdString(track.GetName());
    for (int index = 0; index < track.GetNumLabels(); ++index) {
        const auto* label = track.GetLabel(index);
        if (!label || label->getT1() < start || label->getT0() > end) {
            continue;
        }
        result.labels.push_back({
                label->GetId(),
                label->getT0(),
                label->getT1(),
                au3::wxToStdString(label->title),
                label->GetSelected(),
            });
    }
    return result;
}

void throwProjectError(QObject* object, const QString& message)
{
    if (auto* engine = qjsEngine(object)) {
        engine->throwError(message);
    }
}

QObject* makeCurrentProjectSelection(
    AudacityProject* project, double start, double end, const ClipKeyList& selectedClips, QObject* parent)
{
    return detail::makeProjectSelection(detail::currentState(project, start, end, selectedClips, false), parent);
}

QObject* beginRootProjectEdit(
    AudacityProject* project, double start, double end, const ClipKeyList& selectedClips, const QString& name, IProjectHistoryPtr history,
    std::function<bool()> projectAvailable, std::function<void(double, double, const std::vector<int64_t>&)> setSelection, QObject* parent)
{
    auto state = detail::currentState(project, start, end, selectedClips, true);
    state->undoName = name.toStdString();
    state->history = std::move(history);
    state->projectAvailable = std::move(projectAvailable);
    state->setSelection = std::move(setSelection);
    state->editStarted = true;
    try {
        if (!state->history) {
            throwProjectError(parent, QStringLiteral("Project history is unavailable"));
            return nullptr;
        }
        if (!state->acquire()) {
            throwProjectError(parent, QString::fromStdString(state->error));
            return nullptr;
        }
        state->transaction = std::make_unique<TransactionScope>(*state->project, "ProjectEdit");
        auto* edit = detail::makeProjectEdit(std::move(state), nullptr);
        QJSEngine::setObjectOwnership(edit, QJSEngine::JavaScriptOwnership);
        return edit;
    } catch (const std::exception& exception) {
        throwProjectError(parent, QString::fromUtf8(exception.what()));
    } catch (...) {
        throwProjectError(parent, QStringLiteral("Could not begin the project edit"));
    }
    return nullptr;
}

QObject* ProjectEditSession::beginEdit(QObject* parent)
{
    auto state = m_state;
    if (!state->active) {
        throwProjectError(parent, QString::fromStdString(state->error.empty() ? "The project edit is no longer active" : state->error));
        return nullptr;
    }
    if (state->editStarted) {
        throwProjectError(parent, QStringLiteral("Only one project edit may be active"));
        return nullptr;
    }
    state->editStarted = true;
    auto* edit = detail::makeProjectEdit(std::move(state), nullptr);
    QJSEngine::setObjectOwnership(edit, QJSEngine::JavaScriptOwnership);
    return edit;
}
} // namespace au::trackedit::api
