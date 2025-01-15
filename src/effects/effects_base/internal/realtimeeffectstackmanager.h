/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effectstypes.h"
#include "async/channel.h"

namespace au::effects {
class StackManager
{
public:
    void insert(TrackId trackId, EffectChainLinkIndex index, const RealtimeEffectStatePtr& state);
    void remove(const RealtimeEffectStatePtr& state);
    void replace(const RealtimeEffectStatePtr& oldState, const RealtimeEffectStatePtr& newState);
    void clear();
    void refresh(TrackId trackId, const std::vector<RealtimeEffectStatePtr>& newStates);

    std::optional<TrackId> trackId(const RealtimeEffectStatePtr& state) const;

    muse::async::Channel<TrackId, EffectChainLinkIndex, RealtimeEffectStatePtr> realtimeEffectAdded;
    muse::async::Channel<TrackId, RealtimeEffectStatePtr> realtimeEffectRemoved;
    muse::async::Channel<TrackId, EffectChainLinkIndex, RealtimeEffectStatePtr, RealtimeEffectStatePtr> realtimeEffectReplaced;

private:
    using Stack = std::vector<RealtimeEffectStatePtr>;
    using TrackStacks = std::unordered_map<TrackId, Stack>;

    TrackStacks::const_iterator findStack(const RealtimeEffectStatePtr& state) const;
    TrackStacks::iterator findStack(const RealtimeEffectStatePtr& state);

    // Models how effects are stacked on a track, i.e., the order of the effects in the vectors matters.
    TrackStacks m_stacks;
};
}
