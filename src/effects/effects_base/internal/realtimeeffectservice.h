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
#include <unordered_set>
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
    muse::Inject<IEffectsProvider> effectsProvider;

public:
    void init();

    RealtimeEffectStatePtr addRealtimeEffect(TrackId, const EffectId&) override;
    void removeRealtimeEffect(TrackId, const RealtimeEffectStatePtr&) override;
    RealtimeEffectStatePtr replaceRealtimeEffect(TrackId, const RealtimeEffectStatePtr&, const EffectId& newEffectId) override;
    RealtimeEffectStatePtr replaceRealtimeEffect(TrackId, int effectListIndex, const EffectId& newEffectId) override;
    void moveRealtimeEffect(const RealtimeEffectStatePtr& state, int newIndex) override;

    muse::async::Channel<TrackId, RealtimeEffectStatePtr> realtimeEffectAdded() const override;
    muse::async::Channel<TrackId, RealtimeEffectStatePtr> realtimeEffectRemoved() const override;
    muse::async::Channel<TrackId, EffectChainLinkIndex, RealtimeEffectStatePtr> realtimeEffectReplaced() const override;
    muse::async::Channel<TrackId, EffectChainLinkIndex, EffectChainLinkIndex> realtimeEffectMoved() const override;
    muse::async::Channel<TrackId> realtimeEffectStackChanged() const override;

    std::optional<TrackId> trackId(const RealtimeEffectStatePtr&) const override;
    std::optional<std::string> effectTrackName(const RealtimeEffectStatePtr& state) const override;
    std::optional<std::vector<RealtimeEffectStatePtr> > effectStack(TrackId trackId) const override;

    bool isActive(const RealtimeEffectStatePtr&) const override;
    void setIsActive(const RealtimeEffectStatePtr&, bool) override;
    muse::async::Channel<RealtimeEffectStatePtr> isActiveChanged() const override;

    bool trackEffectsActive(TrackId trackId) const override;
    void setTrackEffectsActive(TrackId trackId, bool active) override;

private:
    void onProjectChanged(const au::project::IAudacityProjectPtr& project);
    void registerRealtimeEffectList(TrackId, RealtimeEffectList&);
    void unregisterRealtimeEffectList(TrackId);
    void onTrackListEvent(const TrackListEvent&);
    std::string getEffectName(const RealtimeEffectState& state) const;
    std::optional<std::string> effectTrackName(TrackId trackId) const;
    RealtimeEffectStatePtr effect(TrackId trackId, EffectChainLinkIndex index) const;
    const RealtimeEffectList* realtimeEffectList(au::effects::TrackId) const;
    RealtimeEffectList* realtimeEffectList(au::effects::TrackId);

    muse::async::Channel<RealtimeEffectStatePtr> m_isActiveChanged;
    Observer::Subscription m_tracklistSubscription;
    std::unordered_map<TrackId, Observer::Subscription> m_rtEffectSubscriptions;
    muse::async::Channel<TrackId, RealtimeEffectStatePtr> m_realtimeEffectAdded;
    muse::async::Channel<TrackId, RealtimeEffectStatePtr> m_realtimeEffectRemoved;
    muse::async::Channel<TrackId, EffectChainLinkIndex, RealtimeEffectStatePtr> m_realtimeEffectReplaced;
    muse::async::Channel<TrackId, EffectChainLinkIndex, EffectChainLinkIndex> m_realtimeEffectMoved;
    muse::async::Channel<TrackId> m_realtimeEffectStackChanged;
    bool m_trackUndoRedoOngoing = false;
    std::unordered_set<TrackId> m_modifiedTracks;
};
}
