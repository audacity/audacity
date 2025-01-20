/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectstackmanager.h"

namespace au::effects {
void StackManager::refresh(TrackId trackId, const std::vector<RealtimeEffectStatePtr>& newStates)
{
    auto& stack = m_stacks[trackId];

    // Store in a vector not to modify the stack size while iterating.
    std::vector<RealtimeEffectStatePtr> removedStates;
    for (auto i = 0u; i < stack.size(); ++i) {
        const auto& state = stack[i];
        if (std::find(newStates.begin(), newStates.end(), state) == newStates.end()) {
            removedStates.push_back(state);
        }
    }
    for (const auto& state : removedStates) {
        remove(state);
    }

    for (auto i = 0u; i < newStates.size(); ++i) {
        const auto& state = newStates[i];
        if (std::find(stack.begin(), stack.end(), state) == stack.end()) {
            insert(trackId, i, state);
        }
    }
}

void StackManager::insert(TrackId trackId, EffectChainLinkIndex index, const RealtimeEffectStatePtr& state)
{
    auto& stack = m_stacks[trackId];
    if (index > stack.size()) {
        stack.resize(index);
    }
    stack.insert(stack.begin() + index, state);
    realtimeEffectAdded.send(trackId, index, state);
}

void StackManager::remove(const RealtimeEffectStatePtr& state)
{
    const auto stackIt = findStack(state);
    if (stackIt == m_stacks.end()) {
        return;
    }
    const TrackId trackId = stackIt->first;
    Stack& stack = stackIt->second;
    const auto it = std::find(stack.begin(), stack.end(), state);
    stack.erase(it);
    realtimeEffectRemoved.send(trackId, state);
}

void StackManager::replace(const RealtimeEffectStatePtr& oldState, const RealtimeEffectStatePtr& newState)
{
    const auto stackIt = findStack(oldState);
    if (stackIt == m_stacks.end()) {
        return;
    }
    const TrackId trackId = stackIt->first;
    Stack& stack = stackIt->second;
    const auto it = std::find(stack.begin(), stack.end(), oldState);
    const auto index = it - stack.begin();
    stack[index] = newState;
    realtimeEffectReplaced.send(trackId, index, oldState, newState);
}

void StackManager::clear()
{
    auto stackIt = m_stacks.begin();
    while (stackIt != m_stacks.end()) {
        const TrackId trackId = stackIt->first;
        auto& stack = stackIt->second;
        for (auto i = 0u; i < stack.size(); ++i) {
            realtimeEffectRemoved.send(trackId, stack[i]);
        }
        stackIt = m_stacks.erase(stackIt);
    }
}

std::optional<TrackId> StackManager::trackId(const RealtimeEffectStatePtr& state) const
{
    const auto stackIt = findStack(state);
    if (stackIt == m_stacks.end()) {
        return {};
    }
    return stackIt->first;
}

StackManager::TrackStacks::const_iterator StackManager::findStack(const RealtimeEffectStatePtr& state) const
{
    return std::find_if(m_stacks.begin(), m_stacks.end(), [state](const auto& pair) {
        const Stack& stack = pair.second;
        return std::find(stack.begin(), stack.end(), state) != stack.end();
    });
}

StackManager::TrackStacks::iterator StackManager::findStack(const RealtimeEffectStatePtr& state)
{
    return std::find_if(m_stacks.begin(), m_stacks.end(), [state](const auto& pair) {
        const Stack& stack = pair.second;
        return std::find(stack.begin(), stack.end(), state) != stack.end();
    });
}
}
