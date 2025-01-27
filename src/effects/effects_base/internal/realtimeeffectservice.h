/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "modularity/ioc.h"
#include "async/asyncable.h"
#include "irealtimeeffectservice.h"
#include "effects/effects_base/ieffectsprovider.h"
#include "effectstypes.h"
#include "realtimeeffectstackmanager.h"
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
    RealtimeEffectService();

    void init();

    RealtimeEffectStatePtr addRealtimeEffect(TrackId, const EffectId&) override;
    void removeRealtimeEffect(TrackId, const RealtimeEffectStatePtr&) override;
    RealtimeEffectStatePtr replaceRealtimeEffect(TrackId, int effectListIndex, const EffectId& newEffectId) override;

    muse::async::Channel<TrackId, EffectChainLinkIndex, RealtimeEffectStatePtr> realtimeEffectAdded() const override;
    muse::async::Channel<TrackId, RealtimeEffectStatePtr> realtimeEffectRemoved() const override;
    muse::async::Channel<TrackId, EffectChainLinkIndex, RealtimeEffectStatePtr,
                         RealtimeEffectStatePtr> realtimeEffectReplaced() const override;

    std::optional<TrackId> trackId(const RealtimeEffectStatePtr&) const override;
    std::optional<std::string> effectTrackName(const RealtimeEffectStatePtr& state) const override;

    bool isActive(const RealtimeEffectStatePtr&) const override;
    void setIsActive(const RealtimeEffectStatePtr&, bool) override;
    muse::async::Channel<RealtimeEffectStatePtr> isActiveChanged() const override;

    bool trackEffectsActive(TrackId trackId) const override;
    void setTrackEffectsActive(TrackId trackId, bool active) override;

private:
    void updateSubscriptions(const au::project::IAudacityProjectPtr& project);
    Observer::Subscription subscribeToRealtimeEffectList(TrackId trackId, RealtimeEffectList&);
    void onTrackListEvent(const TrackListEvent&);
    void onWaveTrackAdded(WaveTrack&);
    std::string getEffectName(const std::string& effectId) const;
    std::string getEffectName(const RealtimeEffectState& state) const;
    std::string getTrackName(const au::au3::Au3Project& project, au::effects::TrackId trackId) const;
    std::optional<std::string> effectTrackName(TrackId trackId) const;
    const RealtimeEffectList* realtimeEffectList(au::effects::TrackId) const;
    RealtimeEffectList* realtimeEffectList(au::effects::TrackId);
    void initializeTrackOnStackManager(au::effects::TrackId trackId);
    void onUndoRedo();

    struct UtilData
    {
        au::au3::Au3Project* const au3Project;
        au::au3::Au3Track* const au3Track;
        trackedit::ITrackeditProject* const trackeditProject;
        RealtimeEffectList* const effectList;
    };

    std::optional<UtilData> utilData(TrackId) const;
    muse::async::Channel<RealtimeEffectStatePtr> m_isActiveChanged;

    Observer::Subscription m_tracklistSubscription;
    std::unordered_map<TrackId, Observer::Subscription> m_rtEffectSubscriptions;

    const std::unique_ptr<StackManager> m_stackManager;
};
}
