#include "realtimeeffectservice.h"

#include "mastereffectundoredo.h"
#include "realtimeeffectserviceutils.h"

#include "libraries/lib-audio-io/AudioIO.h"

#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-effects/EffectManager.h"
#include "libraries/lib-realtime-effects/RealtimeEffectState.h"
#include "libraries/lib-realtime-effects/RealtimeEffectList.h"

#include "project/iaudacityproject.h"

#include "global/types/translatablestring.h"

namespace au::effects {
/*!
 * Note on realtime effect list updates.
 *
 * Synchronization of all realtime effect lists (for regular tracks as well as the master track)
 * require handling events issued by a variety of actions:
 *
 * - Project opened or closed
 * - Track added/removed, because user added/removed a track, undo or redo was called, or e.g. a destructive effect was applied
 * - A realtime effect was added/moved/replaced/removed
 *
 * Track realtime effects are attached to WaveTrack objects, so whenever there is an undo/redo,
 * the track realtime effect lists are updated and we just need to notify that there was a change.
 * For the master effects, undo and redo are specially handled by the `MasterEffectListRestorer`.
 *
 * The RealtimeEffectList assignment operator (RealtimeEffectList::operator=) is on purpose silent ;
 * only once the assignment is finished do we send a notification.
 * This allows realtime effect list UI models to not modify things unnecessarily (like closing a realtime effect dialog)
 */

void RealtimeEffectService::init()
{
    globalContext()->currentProjectChanged().onNotify(this, [this]
    { onProjectChanged(globalContext()->currentProject()); });

    onProjectChanged(globalContext()->currentProject());
}

void RealtimeEffectService::onProjectChanged(const au::project::IAudacityProjectPtr& project)
{
    m_rtEffectSubscriptions.clear();

    if (!project) {
        return;
    }

    auto au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());

    setNotificationChannelForMasterEffectUndoRedo(*au3Project, m_realtimeEffectStackChanged);

    auto& trackList = TrackList::Get(*au3Project);

    for (WaveTrack* track : trackList.Any<WaveTrack>()) {
        registerRealtimeEffectList(track->GetId(), RealtimeEffectList::Get(*track));
    }
    registerRealtimeEffectList(masterTrackId, RealtimeEffectList::Get(*au3Project));

    m_tracklistSubscription = trackList.Subscribe([this, w = weak_from_this()](const TrackListEvent& e)
    {
        const auto lifeguard = w.lock();
        if (lifeguard) {
            onTrackListEvent(e);
        }
    });
}

void RealtimeEffectService::registerRealtimeEffectList(TrackId trackId, RealtimeEffectList& list)
{
    m_rtEffectSubscriptions[trackId] = list.Subscribe([this, trackId, weakThis = weak_from_this()](const RealtimeEffectListMessage& msg)
    {
        const auto lifeguard = weakThis.lock();
        if (!lifeguard) {
            return;
        }
        switch (msg.type) {
            case RealtimeEffectListMessage::Type::Insert:
                m_realtimeEffectAdded.send(trackId, msg.affectedState);
                return;
            case RealtimeEffectListMessage::Type::Remove:
                m_realtimeEffectRemoved.send(trackId, msg.affectedState);
                return;
            case RealtimeEffectListMessage::Type::DidReplace: {
                const auto newState = effect(trackId, msg.srcIndex);
                IF_ASSERT_FAILED(newState) {
                    return;
                }
                m_realtimeEffectReplaced.send(trackId, msg.srcIndex, newState);
            }
                return;
            case RealtimeEffectListMessage::Type::Move:
                m_realtimeEffectMoved.send(trackId, msg.srcIndex, msg.dstIndex);
                return;
        }
    });

    if (m_trackUndoRedoOngoing) {
        m_modifiedTracks.insert(trackId);
    } else {
        m_realtimeEffectStackChanged.send(trackId);
    }
}

void RealtimeEffectService::unregisterRealtimeEffectList(TrackId trackId)
{
    m_rtEffectSubscriptions.erase(trackId);
    if (m_trackUndoRedoOngoing) {
        m_modifiedTracks.insert(trackId);
    } else {
        m_realtimeEffectStackChanged.send(trackId);
    }
}

void RealtimeEffectService::onTrackListEvent(const TrackListEvent& e)
{
    switch (e.mType) {
    case TrackListEvent::ADDITION: {
        const auto waveTrack = std::dynamic_pointer_cast<WaveTrack>(e.mpTrack.lock());
        if (!waveTrack) {
            // TODO @saintmatthieu I've seen these transitory events but haven't looked for the cause.
            return;
        }
        registerRealtimeEffectList(waveTrack->GetId(), RealtimeEffectList::Get(*waveTrack));
    }
    break;
    case TrackListEvent::DELETION:
    {
        IF_ASSERT_FAILED(e.mId.has_value()) {
            return;
        }
        unregisterRealtimeEffectList(*e.mId);
    }
    break;
    case TrackListEvent::UNDO_REDO_BEGIN:
        m_trackUndoRedoOngoing = true;
        break;
    case TrackListEvent::UNDO_REDO_END:
        for (const auto& trackId : m_modifiedTracks) {
            m_realtimeEffectStackChanged.send(trackId);
        }
        m_modifiedTracks.clear();
        m_trackUndoRedoOngoing = false;
        break;
    }
}

muse::async::Channel<TrackId, RealtimeEffectStatePtr> RealtimeEffectService::realtimeEffectAdded() const
{
    return m_realtimeEffectAdded;
}

muse::async::Channel<TrackId, RealtimeEffectStatePtr> RealtimeEffectService::realtimeEffectRemoved() const
{
    return m_realtimeEffectRemoved;
}

muse::async::Channel<TrackId, EffectChainLinkIndex, RealtimeEffectStatePtr> RealtimeEffectService::realtimeEffectReplaced() const
{
    return m_realtimeEffectReplaced;
}

muse::async::Channel<TrackId, EffectChainLinkIndex, EffectChainLinkIndex> RealtimeEffectService::realtimeEffectMoved() const
{
    return m_realtimeEffectMoved;
}

muse::async::Channel<TrackId> RealtimeEffectService::realtimeEffectStackChanged() const
{
    return m_realtimeEffectStackChanged;
}

std::optional<TrackId> RealtimeEffectService::trackId(const RealtimeEffectStatePtr& state) const
{
    return utils::trackId(globalContext()->currentProject(), state);
}

std::optional<std::string> RealtimeEffectService::effectTrackName(const RealtimeEffectStatePtr& state) const
{
    const auto tId = trackId(state);
    if (!tId.has_value()) {
        return {};
    }
    return effectTrackName(*tId);
}

std::optional<std::vector<RealtimeEffectStatePtr> > RealtimeEffectService::effectStack(TrackId trackId) const
{
    const auto data = utils::utilData(globalContext()->currentProject(), trackId);
    if (!data) {
        return {};
    }
    std::vector<RealtimeEffectStatePtr> stack(data->effectList->GetStatesCount());
    for (auto i = 0; i < static_cast<int>(stack.size()); ++i) {
        stack[i] = data->effectList->GetStateAt(i);
    }
    return stack;
}

std::optional<std::string> RealtimeEffectService::effectTrackName(TrackId trackId) const
{
    if (trackId == masterTrackId) {
        return muse::TranslatableString("effects", "Master").translated().toStdString();
    }
    const auto trackeditProject = globalContext()->currentTrackeditProject();
    if (trackeditProject) {
        return trackeditProject->trackName(trackId);
    } else {
        return {};
    }
}

RealtimeEffectStatePtr RealtimeEffectService::effect(TrackId trackId, EffectChainLinkIndex index) const
{
    const auto data = utils::utilData(globalContext()->currentProject(), trackId);
    if (!data) {
        return nullptr;
    }
    if (index < 0 || index >= data->effectList->GetStatesCount()) {
        return nullptr;
    }
    return data->effectList->GetStateAt(index);
}

RealtimeEffectStatePtr RealtimeEffectService::addRealtimeEffect(TrackId trackId, const muse::String& effectId)
{
    const auto data = utils::utilData(globalContext()->currentProject(), trackId);
    IF_ASSERT_FAILED(data) {
        return nullptr;
    }

    if (const auto state = AudioIO::Get()->AddState(*data->au3Project, data->au3Track, effectId.toStdString())) {
        const auto effectName = getEffectName(*state);
        const auto trackName = effectTrackName(trackId);
        projectHistory()->pushHistoryState("Added " + effectName + " to " + trackName.value_or(""), "Add " + effectName);
        return state;
    }

    return nullptr;
}

namespace {
std::shared_ptr<RealtimeEffectState> findEffectState(RealtimeEffectList& effectList, RealtimeEffectStatePtr effectStateId)
{
    for (auto i = 0; i < effectList.GetStatesCount(); ++i) {
        const auto state = effectList.GetStateAt(i);
        if (state == effectStateId) {
            return state;
        }
    }
    return nullptr;
}
}

void RealtimeEffectService::removeRealtimeEffect(TrackId trackId, const RealtimeEffectStatePtr& effectStateId)
{
    const auto data = utils::utilData(globalContext()->currentProject(), trackId);
    IF_ASSERT_FAILED(data) {
        return;
    }

    const auto state = findEffectState(*data->effectList, effectStateId);
    IF_ASSERT_FAILED(state) {
        return;
    }

    AudioIO::Get()->RemoveState(*data->au3Project, data->au3Track, state);
    const auto effectName = getEffectName(*state);
    const auto trackName = effectTrackName(trackId);
    projectHistory()->pushHistoryState("Removed " + effectName + " from " + trackName.value_or(""), "Remove " + effectName);
}

RealtimeEffectStatePtr RealtimeEffectService::replaceRealtimeEffect(TrackId trackId, int effectListIndex, const muse::String& newEffectId)
{
    const auto data = utils::utilData(globalContext()->currentProject(), trackId);
    IF_ASSERT_FAILED(data) {
        return nullptr;
    }

    const auto oldState = data->effectList->GetStateAt(effectListIndex);
    if (const auto newState = AudioIO::Get()->ReplaceState(*data->au3Project, data->au3Track, effectListIndex, newEffectId.toStdString())) {
        const auto oldEffectName = getEffectName(*oldState);
        const auto newEffectName = getEffectName(*newState);
        projectHistory()->pushHistoryState("Replaced " + oldEffectName + " with " + newEffectName, "Replace " + oldEffectName);
        return newState;
    }

    return nullptr;
}

RealtimeEffectStatePtr RealtimeEffectService::replaceRealtimeEffect(TrackId trackId, const RealtimeEffectStatePtr& state,
                                                                    const muse::String& newEffectId)
{
    const auto index = utils::effectIndex(globalContext()->currentProject(), state);
    if (!index.has_value()) {
        return nullptr;
    }
    return replaceRealtimeEffect(trackId, *index, newEffectId);
}

void RealtimeEffectService::moveRealtimeEffect(const RealtimeEffectStatePtr& state, int newIndex)
{
    const auto tId = trackId(state);
    if (!tId.has_value()) {
        return;
    }
    const auto oldIndex = utils::effectIndex(globalContext()->currentProject(), state);
    IF_ASSERT_FAILED(oldIndex.has_value()) {
        return;
    }
    RealtimeEffectList* const list = realtimeEffectList(*tId);
    list->MoveEffect(*oldIndex, newIndex);

    const auto effectName = getEffectName(*state);
    const auto trackName = effectTrackName(*tId);
    projectHistory()->pushHistoryState("Moved " + effectName
                                       + (oldIndex < newIndex ? " up " : " down ") + " in " + trackName.value_or(""),
                                       "Change effect order");
}

bool RealtimeEffectService::isActive(const RealtimeEffectStatePtr& stateId) const
{
    if (stateId == 0) {
        return false;
    }
    return stateId->GetSettings().extra.GetActive();
}

void RealtimeEffectService::setIsActive(const RealtimeEffectStatePtr& stateId, bool isActive)
{
    if (stateId->GetSettings().extra.GetActive() == isActive) {
        return;
    }
    stateId->SetActive(isActive);
    m_isActiveChanged.send(stateId);
}

muse::async::Channel<RealtimeEffectStatePtr> RealtimeEffectService::isActiveChanged() const
{
    return m_isActiveChanged;
}

bool RealtimeEffectService::trackEffectsActive(TrackId trackId) const
{
    const auto list = realtimeEffectList(trackId);
    return list ? list->IsActive() : false;
}

void RealtimeEffectService::setTrackEffectsActive(TrackId trackId, bool active)
{
    const auto list = realtimeEffectList(trackId);
    if (list) {
        list->SetActive(active);
    }
}

const RealtimeEffectList* RealtimeEffectService::realtimeEffectList(TrackId trackId) const
{
    const auto data = utils::utilData(globalContext()->currentProject(), trackId);
    if (!data) {
        return nullptr;
    }
    return data->effectList;
}

RealtimeEffectList* RealtimeEffectService::realtimeEffectList(TrackId trackId)
{
    return const_cast<RealtimeEffectList*>(const_cast<const RealtimeEffectService*>(this)->realtimeEffectList(trackId));
}

std::string RealtimeEffectService::getEffectName(const RealtimeEffectState& state) const
{
    return effectsProvider()->effectName(state.GetID().ToStdString());
}
}

// Inject a factory for realtime effects
static RealtimeEffectState::EffectFactory::Scope scope{ &EffectManager::GetInstanceFactory };
