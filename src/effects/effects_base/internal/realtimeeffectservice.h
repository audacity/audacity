/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>
#include <unordered_map>
#include "async/asyncable.h"
#include "context/iglobalcontext.h"
#include "effects/effects_base/ieffectsprovider.h"
#include "effectstypes.h"
#include "irealtimeeffectservice.h"
#include "libraries/lib-utility/Observer.h"
#include "modularity/ioc.h"
#include "trackedit/iprojecthistory.h"

struct TrackListEvent;

class AudacityProject;
class RealtimeEffectList;
class WaveTrack;
class RealtimeEffectState;
struct TrackListEvent;

namespace au3 {
using Au3Project = ::AudacityProject;
}

namespace au::effects {
class RealtimeEffectService : public IRealtimeEffectService,
                              muse::async::Asyncable,
                              public std::enable_shared_from_this<RealtimeEffectService>
{
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<trackedit::IProjectHistory> projectHistory;
    muse::Inject<effects::IEffectsProvider> effectsProvider;

public:
    void init();

    void addRealtimeEffect(project::IAudacityProject&, TrackId, const EffectId&) override;
    void removeRealtimeEffect(project::IAudacityProject&, TrackId, EffectStateId) override;
    void replaceRealtimeEffect(project::IAudacityProject&, TrackId, int effectListIndex, const EffectId& newEffectId) override;

    muse::async::Channel<TrackId, EffectChainLinkIndex, EffectChainLinkPtr> realtimeEffectAdded() const override;
    muse::async::Channel<TrackId, EffectChainLinkIndex, EffectChainLinkPtr> realtimeEffectRemoved() const override;
    muse::async::Channel<TrackId, EffectChainLinkIndex, EffectChainLinkPtr, EffectChainLinkPtr> realtimeEffectReplaced() const override;

private:
    Observer::Subscription subscribeToRealtimeEffectList(WaveTrack&, RealtimeEffectList&);
    void onTrackListEvent(const TrackListEvent&);
    void onWaveTrackAdded(WaveTrack&);
    std::string getEffectName(const std::string& effectId) const;
    std::string getEffectName(const RealtimeEffectState& state) const;
    std::string getTrackName(const au::au3::Au3Project& project, au::effects::TrackId trackId) const;

    muse::async::Channel<TrackId, EffectChainLinkIndex, EffectChainLinkPtr> m_realtimeEffectAdded;
    muse::async::Channel<TrackId, EffectChainLinkIndex, EffectChainLinkPtr> m_realtimeEffectRemoved;
    muse::async::Channel<TrackId, EffectChainLinkIndex, EffectChainLinkPtr, EffectChainLinkPtr> m_realtimeEffectReplaced;

    Observer::Subscription m_tracklistSubscription;
    std::unordered_map<TrackId, Observer::Subscription> m_rtEffectSubscriptions;
};
}  // namespace au::effects
