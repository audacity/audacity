/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"
#include "async/asyncable.h"
#include "irealtimeeffectservice.h"
#include "effects/effects_base/ieffectsprovider.h"
#include "effectstypes.h"
#include "context/iglobalcontext.h"
#include "trackedit/iprojecthistory.h"
#include "libraries/lib-utility/Observer.h"
#include <unordered_map>
#include <memory>

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
class RealtimeEffectService : public IRealtimeEffectService, muse::async::Asyncable,
    public std::enable_shared_from_this<RealtimeEffectService>
{
    muse::Inject<context::IGlobalContext> globalContext;
    muse::Inject<trackedit::IProjectHistory> projectHistory;
    muse::Inject<effects::IEffectsProvider> effectsProvider;

public:
    void init();

    RealtimeEffectStatePtr addRealtimeEffect(TrackId, const EffectId&) override;
    void removeRealtimeEffect(TrackId, EffectStateId) override;
    RealtimeEffectStatePtr replaceRealtimeEffect(TrackId, int effectListIndex, const EffectId& newEffectId) override;

    muse::async::Channel<TrackId, EffectChainLinkIndex, EffectStateId> realtimeEffectAdded() const override;
    muse::async::Channel<TrackId, EffectStateId> realtimeEffectRemoved() const override;
    muse::async::Channel<TrackId, EffectChainLinkIndex, EffectStateId, EffectStateId> realtimeEffectReplaced() const override;

    std::optional<TrackId> trackId(EffectStateId) const override;

    bool isActive(EffectStateId) const override;
    void setIsActive(EffectStateId, bool) override;
    muse::async::Channel<EffectStateId> isActiveChanged() const override;

    bool trackEffectsActive(TrackId trackId) const override;
    void setTrackEffectsActive(TrackId trackId, bool active) override;

private:
    void updateSubscriptions(const au::project::IAudacityProjectPtr& project);
    Observer::Subscription subscribeToRealtimeEffectList(WaveTrack&, RealtimeEffectList&);
    void onTrackListEvent(const TrackListEvent&);
    void onWaveTrackAdded(WaveTrack&);
    std::string getEffectName(const std::string& effectId) const;
    std::string getEffectName(const RealtimeEffectState& state) const;
    std::string getTrackName(const au::au3::Au3Project& project, au::effects::TrackId trackId) const;
    const RealtimeEffectList* realtimeEffectList(au::effects::TrackId) const;
    RealtimeEffectList* realtimeEffectList(au::effects::TrackId);

    struct UtilData {
        au::au3::Au3Project* const au3Project;
        au::au3::Au3Track* const au3Track;
        trackedit::ITrackeditProject* const trackeditProject;
    };

    std::optional<UtilData> utilData(TrackId) const;

    muse::async::Channel<TrackId, EffectChainLinkIndex, EffectStateId> m_realtimeEffectAdded;
    muse::async::Channel<TrackId, EffectStateId> m_realtimeEffectRemoved;
    muse::async::Channel<TrackId, EffectChainLinkIndex, EffectStateId, EffectStateId> m_realtimeEffectReplaced;
    muse::async::Channel<EffectStateId> m_isActiveChanged;

    Observer::Subscription m_tracklistSubscription;
    std::unordered_map<TrackId, Observer::Subscription> m_rtEffectSubscriptions;
    std::unordered_map<EffectStateId, TrackId> m_effectTrackMap;
};
}
