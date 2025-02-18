/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "async/notification.h"
#include "async/channel.h"
#include "global/modularity/imoduleinterface.h"
#include "effectstypes.h"
#include <optional>

struct TransportSequences;
struct AudioIOStartStreamOptions;

namespace au::project {
class IAudacityProject;
}

namespace au::effects {
class IRealtimeEffectService : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IRealtimeEffectService);

public:
    static constexpr TrackId masterTrackId = -2;

    virtual ~IRealtimeEffectService() = default;

    virtual RealtimeEffectStatePtr addRealtimeEffect(TrackId trackId, const EffectId& effectId) = 0;
    virtual void removeRealtimeEffect(TrackId trackId, const RealtimeEffectStatePtr& state) = 0;
    virtual RealtimeEffectStatePtr replaceRealtimeEffect(TrackId trackId, int effectListIndex, const EffectId& newEffectId) = 0;
    virtual RealtimeEffectStatePtr replaceRealtimeEffect(TrackId trackId, const RealtimeEffectStatePtr& state,
                                                         const EffectId& newEffectId) = 0;
    virtual void moveRealtimeEffect(const RealtimeEffectStatePtr& state, int newIndex) = 0;

    virtual muse::async::Channel<TrackId, RealtimeEffectStatePtr> realtimeEffectAdded() const = 0;
    virtual muse::async::Channel<TrackId, RealtimeEffectStatePtr> realtimeEffectRemoved() const = 0;
    virtual muse::async::Channel<TrackId, EffectChainLinkIndex, RealtimeEffectStatePtr> realtimeEffectReplaced() const = 0;
    virtual muse::async::Channel<TrackId, EffectChainLinkIndex, EffectChainLinkIndex> realtimeEffectMoved() const = 0;
    virtual muse::async::Channel<TrackId> realtimeEffectStackChanged() const = 0;

    virtual std::optional<TrackId> trackId(const RealtimeEffectStatePtr& state) const = 0;
    virtual std::optional<std::string> effectTrackName(const RealtimeEffectStatePtr& state) const = 0;
    virtual std::optional<std::vector<RealtimeEffectStatePtr> > effectStack(TrackId trackId) const = 0;

    virtual bool isActive(const RealtimeEffectStatePtr& state) const = 0;
    virtual void setIsActive(const RealtimeEffectStatePtr& state, bool) = 0;
    virtual muse::async::Channel<RealtimeEffectStatePtr> isActiveChanged() const = 0;

    virtual bool trackEffectsActive(TrackId trackId) const = 0;
    virtual void setTrackEffectsActive(TrackId trackId, bool active) = 0;
};
}
