#include "realtimeeffectservice.h"

#include "libraries/lib-audio-io/AudioIO.h"
#include "libraries/lib-audio-io/ProjectAudioIO.h"

#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-time-frequency-selection/ViewInfo.h"
#include "libraries/lib-effects/EffectManager.h"
#include "libraries/lib-realtime-effects/RealtimeEffectState.h"
#include "libraries/lib-realtime-effects/RealtimeEffectList.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/domconverter.h"

#include "project/iaudacityproject.h"

namespace au::effects {
std::string RealtimeEffectService::getEffectName(const RealtimeEffectState& state) const
{
    return effectsProvider()->effectName(state.GetID().ToStdString());
}

void RealtimeEffectService::init()
{
    globalContext()->currentProjectChanged().onNotify(this, [this]
    {
        updateSubscriptions(globalContext()->currentProject());
    });

    updateSubscriptions(globalContext()->currentProject());
}

void RealtimeEffectService::updateSubscriptions(const au::project::IAudacityProjectPtr& project)
{
    m_rtEffectSubscriptions.clear();
    if (!project) {
        for (const std::pair<RealtimeEffectStatePtr, TrackId>& entry : m_effectTrackMap) {
            m_realtimeEffectRemoved.send(entry.second, entry.first);
        }
        m_effectTrackMap.clear();
        return;
    }
    auto au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    m_tracklistSubscription = TrackList::Get(*au3Project).Subscribe([this, w = weak_from_this()](const TrackListEvent& e) {
        const auto lifeguard = w.lock();
        if (lifeguard) {
            onTrackListEvent(e);
        }
    });
}

void RealtimeEffectService::onTrackListEvent(const TrackListEvent& e)
{
    auto waveTrack = std::dynamic_pointer_cast<WaveTrack>(e.mpTrack.lock());
    if (!waveTrack) {
        return;
    }
    switch (e.mType) {
    case TrackListEvent::ADDITION:
        onWaveTrackAdded(*waveTrack);
        break;
    case TrackListEvent::DELETION:
        m_rtEffectSubscriptions.erase(waveTrack->GetId());
        break;
    }
}

void RealtimeEffectService::onWaveTrackAdded(WaveTrack& track)
{
    const auto isNewTrack = m_rtEffectSubscriptions.count(track.GetId()) == 0;

    // Renew the subscription: `track` may have a registered ID, it may yet be a new object.
    auto& list = RealtimeEffectList::Get(track);
    m_rtEffectSubscriptions[track.GetId()] = subscribeToRealtimeEffectList(track, list);

    if (isNewTrack) {
        // We have subscribed for future realtime-effect events, but we need to send the initial state.
        for (auto i = 0u; i < list.GetStatesCount(); ++i) {
            const auto state = list.GetStateAt(i);
            IF_ASSERT_FAILED(state) {
                break;
            }
            m_effectTrackMap[state] = track.GetId();
            m_realtimeEffectAdded.send(track.GetId(), i, state);
        }
    }
}

Observer::Subscription RealtimeEffectService::subscribeToRealtimeEffectList(WaveTrack& track, RealtimeEffectList& list)
{
    return list.Subscribe([&, weakThis = weak_from_this(), weakTrack = track.weak_from_this()](const RealtimeEffectListMessage& msg)
    {
        const auto lifeguard = weakThis.lock();
        if (!lifeguard) {
            return;
        }
        const auto waveTrack = std::dynamic_pointer_cast<WaveTrack>(weakTrack.lock());
        if (!waveTrack) {
            return;
        }
        const auto trackId = waveTrack->GetId();
        auto& list = RealtimeEffectList::Get(*waveTrack);
        switch (msg.type) {
        case RealtimeEffectListMessage::Type::Insert:
            m_effectTrackMap[msg.affectedState] = trackId;
            m_realtimeEffectAdded.send(trackId, msg.srcIndex, msg.affectedState);
            break;
        case RealtimeEffectListMessage::Type::Remove:
            m_effectTrackMap.erase(msg.affectedState);
            m_realtimeEffectRemoved.send(trackId, msg.affectedState);
            break;
        case RealtimeEffectListMessage::Type::DidReplace:
        {
            const std::shared_ptr<RealtimeEffectState> newState = list.GetStateAt(msg.dstIndex);
            IF_ASSERT_FAILED(newState) {
                return;
            }
            m_effectTrackMap[newState] = trackId;
            m_effectTrackMap.erase(msg.affectedState);
            m_realtimeEffectReplaced.send(trackId, msg.srcIndex, msg.affectedState, newState);
        }
        break;
        }
    });
}

muse::async::Channel<TrackId, EffectChainLinkIndex, RealtimeEffectStatePtr> RealtimeEffectService::realtimeEffectAdded() const
{
    return m_realtimeEffectAdded;
}

muse::async::Channel<TrackId, RealtimeEffectStatePtr> RealtimeEffectService::realtimeEffectRemoved() const
{
    return m_realtimeEffectRemoved;
}

muse::async::Channel<TrackId, EffectChainLinkIndex, RealtimeEffectStatePtr,
                     RealtimeEffectStatePtr> RealtimeEffectService::realtimeEffectReplaced() const
{
    return m_realtimeEffectReplaced;
}

std::optional<TrackId> RealtimeEffectService::trackId(const RealtimeEffectStatePtr& stateId) const
{
    const auto it = std::find_if(m_effectTrackMap.begin(), m_effectTrackMap.end(), [stateId](const auto& entry) {
        return entry.first == stateId;
    });
    if (it == m_effectTrackMap.end()) {
        return {};
    }
    return it->second;
}

std::optional<RealtimeEffectService::UtilData> RealtimeEffectService::utilData(TrackId trackId) const
{
    const project::IAudacityProjectPtr project = globalContext()->currentProject();
    if (!project) {
        return {};
    }

    const auto au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    const auto au3Track = dynamic_cast<au::au3::Au3Track*>(au::au3::DomAccessor::findTrack(*au3Project, au::au3::Au3TrackId(trackId)));
    const auto trackeditProject = globalContext()->currentTrackeditProject().get();

    if (!au3Project || !au3Track || !trackeditProject) {
        return {};
    }
    return UtilData{ au3Project, au3Track, trackeditProject };
}

RealtimeEffectStatePtr RealtimeEffectService::addRealtimeEffect(TrackId trackId, const muse::String& effectId)
{
    const auto data = utilData(trackId);
    IF_ASSERT_FAILED(data) {
        return nullptr;
    }

    const auto state = AudioIO::Get()->AddState(*data->au3Project, data->au3Track, effectId.toStdString());
    if (state) {
        const auto effectName = getEffectName(*state);
        const auto trackName =  data->trackeditProject->trackName(trackId);
        assert(trackName.has_value());
        projectHistory()->pushHistoryState("Added " + effectName + " to " + trackName.value_or(""), "Add " + effectName);
    }

    return state;
}

namespace {
std::shared_ptr<RealtimeEffectState> findEffectState(au::au3::Au3Track& au3Track, RealtimeEffectStatePtr effectStateId)
{
    auto& effectList = RealtimeEffectList::Get(au3Track);
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
    const auto data = utilData(trackId);
    IF_ASSERT_FAILED(data) {
        return;
    }

    const auto state = findEffectState(*data->au3Track, effectStateId);
    IF_ASSERT_FAILED(state) {
        return;
    }

    AudioIO::Get()->RemoveState(*data->au3Project, data->au3Track, state);
    const auto effectName = getEffectName(*state);
    const auto trackName = data->trackeditProject->trackName(trackId);
    assert(trackName.has_value());
    projectHistory()->pushHistoryState("Removed " + effectName + " from " + trackName.value_or(""), "Remove " + effectName);
}

RealtimeEffectStatePtr RealtimeEffectService::replaceRealtimeEffect(TrackId trackId, int effectListIndex, const muse::String& newEffectId)
{
    const auto data = utilData(trackId);
    IF_ASSERT_FAILED(data) {
        return nullptr;
    }

    const auto newState = AudioIO::Get()->ReplaceState(*data->au3Project, data->au3Track, effectListIndex, newEffectId.toStdString());
    const auto oldState = RealtimeEffectList::Get(*data->au3Track).GetStateAt(effectListIndex);
    if (newState && oldState) {
        const auto newEffectName = getEffectName(*newState);
        const auto oldEffectName = getEffectName(*oldState);
        projectHistory()->pushHistoryState("Replaced " + oldEffectName + " with " + newEffectName, "Replace " + oldEffectName);
    }

    return newState;
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
    const auto data = utilData(trackId);
    if (!data) {
        return nullptr;
    }
    return &RealtimeEffectList::Get(*data->au3Track);
}

RealtimeEffectList* RealtimeEffectService::realtimeEffectList(TrackId trackId)
{
    return const_cast<RealtimeEffectList*>(const_cast<const RealtimeEffectService*>(this)->realtimeEffectList(trackId));
}
}

// Inject a factory for realtime effects
static RealtimeEffectState::EffectFactory::Scope scope{ &EffectManager::GetInstanceFactory };
