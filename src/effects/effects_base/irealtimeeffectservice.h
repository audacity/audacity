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
    virtual ~IRealtimeEffectService() = default;

    virtual RealtimeEffectStatePtr addRealtimeEffect(TrackId trackId, const EffectId& effectId) = 0;
    virtual void removeRealtimeEffect(TrackId trackId, EffectStateId stateId) = 0;
    virtual RealtimeEffectStatePtr replaceRealtimeEffect(TrackId trackId, int effectListIndex, const EffectId& newEffectId) = 0;

    virtual muse::async::Channel<TrackId, EffectChainLinkIndex, EffectStateId> realtimeEffectAdded() const = 0;
    virtual muse::async::Channel<TrackId, EffectChainLinkIndex, EffectStateId> realtimeEffectRemoved() const = 0;
    virtual muse::async::Channel<TrackId, EffectChainLinkIndex, EffectStateId, EffectStateId> realtimeEffectReplaced() const = 0;

    virtual std::optional<TrackId> trackId(EffectStateId) const = 0;

    virtual bool isActive(EffectStateId) const = 0;
    virtual void setIsActive(EffectStateId, bool) = 0;
    virtual muse::async::Channel<EffectStateId> isActiveChanged() const = 0;
};
}
