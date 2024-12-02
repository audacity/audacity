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
    globalContext()->currentProjectChanged().onNotify(this, [this] {
        m_rtEffectSubscriptions.clear();
        auto project = globalContext()->currentProject();
        if (!project) {
            return;
        }
        auto au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
        m_tracklistSubscription = TrackList::Get(*au3Project).Subscribe([this, w = weak_from_this()](const TrackListEvent& e) {
            const auto lifeguard = w.lock();
            if (lifeguard) {
                onTrackListEvent(e);
            }
        });
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
    auto& list = RealtimeEffectList::Get(track);
    m_rtEffectSubscriptions[track.GetId()] = subscribeToRealtimeEffectList(track, list);

    // We have subscribed for future realtime-effect events, but we need to send the initial state.
    for (auto i = 0u; i < list.GetStatesCount(); ++i) {
        const auto state = list.GetStateAt(i);
        IF_ASSERT_FAILED(state) {
            break;
        }
        auto newEffect = std::make_shared<EffectChainLink>(getEffectName(*state), reinterpret_cast<EffectStateId>(state.get()));
        m_realtimeEffectAdded.send(track.GetId(), i, std::move(newEffect));
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
        const auto effectStateId = reinterpret_cast<EffectStateId>(msg.affectedState.get());
        switch (msg.type) {
        case RealtimeEffectListMessage::Type::Insert:
        {
            auto newEffect = std::make_shared<EffectChainLink>(getEffectName(*msg.affectedState), effectStateId);
            m_realtimeEffectAdded.send(trackId, msg.srcIndex, std::move(newEffect));
        }
        break;
        case RealtimeEffectListMessage::Type::Remove:
        {
            auto oldEffect = std::make_shared<EffectChainLink>(getEffectName(*msg.affectedState), effectStateId);
            m_realtimeEffectRemoved.send(trackId, msg.srcIndex, std::move(oldEffect));
        }
        break;
        case RealtimeEffectListMessage::Type::DidReplace:
        {
            const std::shared_ptr<RealtimeEffectState> newState = list.GetStateAt(msg.dstIndex);
            IF_ASSERT_FAILED(newState) {
                return;
            }
            auto oldEffect = std::make_shared<EffectChainLink>(getEffectName(*msg.affectedState), effectStateId);
            auto newEffect = std::make_shared<EffectChainLink>(getEffectName(*newState), reinterpret_cast<EffectStateId>(newState.get()));
            m_realtimeEffectReplaced.send(trackId, msg.srcIndex, std::move(oldEffect), std::move(newEffect));
        }
        break;
        }
    });
}

muse::async::Channel<TrackId, EffectChainLinkIndex, EffectChainLinkPtr> RealtimeEffectService::realtimeEffectAdded() const
{
    return m_realtimeEffectAdded;
}

muse::async::Channel<TrackId, EffectChainLinkIndex, EffectChainLinkPtr> RealtimeEffectService::realtimeEffectRemoved() const
{
    return m_realtimeEffectRemoved;
}

muse::async::Channel<TrackId, EffectChainLinkIndex, EffectChainLinkPtr,
                     EffectChainLinkPtr> RealtimeEffectService::realtimeEffectReplaced() const
{
    return m_realtimeEffectReplaced;
}

namespace {
std::pair<au::au3::Au3Project*, au::au3::Au3Track*> au3ProjectAndTrack(au::project::IAudacityProject& project, TrackId trackId)
{
    auto au3Project = reinterpret_cast<au::au3::Au3Project*>(project.au3ProjectPtr());
    auto au3Track = dynamic_cast<au::au3::Au3Track*>(au::au3::DomAccessor::findTrack(*au3Project, au::au3::Au3TrackId(trackId)));
    return { au3Project, au3Track };
}
}

void RealtimeEffectService::addRealtimeEffect(au::project::IAudacityProject& project, TrackId trackId, const muse::String& effectId)
{
    const auto [au3Project, au3Track] = au3ProjectAndTrack(project, trackId);
    IF_ASSERT_FAILED(au3Project && au3Track) {
        return;
    }

    if (const auto state = AudioIO::Get()->AddState(*au3Project, au3Track, effectId.toStdString())) {
        const auto effectName = getEffectName(*state);
        const auto trackName =  project.trackeditProject()->trackName(trackId);
        projectHistory()->pushHistoryState("Added " + effectName + " to " + trackName, "Add " + effectName);
    }
}

namespace {
std::shared_ptr<RealtimeEffectState> findEffectState(au::au3::Au3Track& au3Track, EffectStateId effectStateId)
{
    auto& effectList = RealtimeEffectList::Get(au3Track);
    for (auto i = 0; i < effectList.GetStatesCount(); ++i) {
        const auto state = effectList.GetStateAt(i);
        if (state.get() == reinterpret_cast<RealtimeEffectState*>(effectStateId)) {
            return state;
        }
    }
    return nullptr;
}
}

void RealtimeEffectService::removeRealtimeEffect(au::project::IAudacityProject& project, TrackId trackId, EffectStateId effectStateId)
{
    const auto [au3Project, au3Track] = au3ProjectAndTrack(project, trackId);
    IF_ASSERT_FAILED(au3Project && au3Track) {
        return;
    }

    const auto state = findEffectState(*au3Track, effectStateId);
    IF_ASSERT_FAILED(state) {
        return;
    }

    AudioIO::Get()->RemoveState(*au3Project, au3Track, state);
    const auto effectName = getEffectName(*state);
    const auto trackName = project.trackeditProject()->trackName(trackId);
    projectHistory()->pushHistoryState("Removed " + effectName + " from " + trackName, "Remove " + effectName);
}

void RealtimeEffectService::replaceRealtimeEffect(au::project::IAudacityProject& project, TrackId trackId, int effectListIndex,
                                                  const muse::String& newEffectId)
{
    const auto [au3Project, au3Track] = au3ProjectAndTrack(project, trackId);
    IF_ASSERT_FAILED(au3Project && au3Track) {
        return;
    }

    const auto newState = AudioIO::Get()->ReplaceState(*au3Project, au3Track, effectListIndex, newEffectId.toStdString());
    const auto oldState = RealtimeEffectList::Get(*au3Track).GetStateAt(effectListIndex);
    if (newState && oldState) {
        const auto newEffectName = getEffectName(*newState);
        const auto oldEffectName = getEffectName(*oldState);
        projectHistory()->pushHistoryState("Replaced " + oldEffectName + " with " + newEffectName, "Replace " + oldEffectName);
    }
}
}

// Inject a factory for realtime effects
static RealtimeEffectState::EffectFactory::Scope scope{ &EffectManager::GetInstanceFactory };
