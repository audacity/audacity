/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "async/notification.h"
#include "async/channel.h"
#include "global/modularity/imoduleinterface.h"
#include "effectstypes.h"

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

    virtual void addRealtimeEffect(project::IAudacityProject& project, TrackId trackId, const EffectId& effectId) = 0;
    virtual void removeRealtimeEffect(project::IAudacityProject& project, TrackId trackId, EffectStateId stateId) = 0;
    virtual void replaceRealtimeEffect(project::IAudacityProject& project, TrackId trackId, int effectListIndex,
                                       const EffectId& newEffectId) = 0;

    virtual muse::async::Channel<TrackId, EffectChainLinkIndex, EffectChainLinkPtr> realtimeEffectAdded() const = 0;
    virtual muse::async::Channel<TrackId, EffectChainLinkIndex, EffectChainLinkPtr> realtimeEffectRemoved() const = 0;
    virtual muse::async::Channel<TrackId, EffectChainLinkIndex, EffectChainLinkPtr, EffectChainLinkPtr> realtimeEffectReplaced() const = 0;
};
}
