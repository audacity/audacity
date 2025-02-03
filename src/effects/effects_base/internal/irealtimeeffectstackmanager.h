/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effectstypes.h"
#include "async/channel.h"
#include "modularity/ioc.h"

namespace au::effects {
class IStackManager : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IStackManager);

public:
    using Stack = std::vector<RealtimeEffectStatePtr>;
    using TrackStacks = std::unordered_map<TrackId, Stack>;

    virtual void insert(TrackId trackId, EffectChainLinkIndex index, const RealtimeEffectStatePtr& state) = 0;
    virtual void remove(const RealtimeEffectStatePtr& state) = 0;
    virtual void remove(TrackId trackId) = 0;
    virtual void replace(const RealtimeEffectStatePtr& oldState, const RealtimeEffectStatePtr& newState) = 0;
    virtual void reorder(const RealtimeEffectStatePtr& state, int newIndex) = 0;

    virtual muse::async::Channel<TrackId> realtimeEffectStackChanged() const = 0;
    virtual std::optional<Stack> effectStack(TrackId) const = 0;

    virtual std::optional<TrackId> trackId(const RealtimeEffectStatePtr& state) const = 0;
    virtual std::optional<int> effectIndex(const RealtimeEffectStatePtr& state) const = 0;
};
}
