/*
 * Audacity: A Digital Audio Editor
 */
#include "realtimeeffectstackmanager.h"
#include "au3wrap/au3types.h"
#include "libraries/lib-project-history/UndoManager.h"
#include "libraries/lib-project/Project.h"
#include "modularity/ioc.h"
#include "log.h"
#include <unordered_set>
#include <stack>

namespace au::effects {
static const AudacityProject::AttachedObjects::RegisteredFactory key{
    [](au3::Au3Project&) {
        return std::make_shared<StackManager>();
    }
};

class EffectStackRestorer final : public UndoStateExtension
{
public:
    EffectStackRestorer(au3::Au3Project& project)
        : m_dstStacks{StackManager::Get(project).m_stacks}
    {
    }

    void RestoreUndoRedoState(au3::Au3Project& project) override
    {
        auto& manager = StackManager::Get(project);
        if (manager.m_stacks == m_dstStacks) {
            return;
        }
        for (const auto& [trackId, dstStack] : m_dstStacks) {
            if (!manager.m_stacks.count(trackId) || manager.m_stacks.at(trackId) != dstStack) {
                manager.m_stacks[trackId] = dstStack;
                manager.m_realtimeEffectStackChanged.send(trackId);
            }
        }
        auto it = manager.m_stacks.begin();
        while (it != manager.m_stacks.end()) {
            const TrackId trackId = it->first;
            if (!m_dstStacks.count(trackId)) {
                it = manager.m_stacks.erase(it);
                manager.m_realtimeEffectStackChanged.send(trackId);
            } else {
                ++it;
            }
        }
    }

    const IStackManager::TrackStacks m_dstStacks;
};

static UndoRedoExtensionRegistry::Entry sEntry {
    [](au3::Au3Project& project) -> std::shared_ptr<EffectStackRestorer>
    {
        return std::make_shared<EffectStackRestorer>(project);
    }
};

StackManager& StackManager::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<StackManager>(key);
}

bool StackManager::insert(TrackId trackId, EffectChainLinkIndex index, const RealtimeEffectStatePtr& state)
{
    const auto stackSize = m_stacks.count(trackId) ? m_stacks.at(trackId).size() : 0;
    if (index > stackSize) {
        return false;
    }
    auto& stack = m_stacks[trackId];
    stack.insert(stack.begin() + index, state);
    return true;
}

bool StackManager::remove(const RealtimeEffectStatePtr& state)
{
    return doRemove(state);
}

std::optional<IStackManager::Stack> StackManager::remove(TrackId trackId)
{
    const auto stackIt = m_stacks.find(trackId);
    if (stackIt == m_stacks.end()) {
        return {};
    }
    // Copy the stack ; erasure may invalidate the iterator.
    IStackManager::Stack prevStack = stackIt->second;
    const auto success = std::all_of(prevStack.begin(), prevStack.end(), [this](const auto& state)
    {
        return doRemove(state);
    });
    IF_ASSERT_FAILED(success)
    {
        // Restore the stack.
        stackIt->second = prevStack;
        return {};
    }

    m_stacks.erase(stackIt);

    return prevStack;
}

bool StackManager::replace(const RealtimeEffectStatePtr& oldState, const RealtimeEffectStatePtr& newState)
{
    const auto stackIt = findStack(oldState);
    if (stackIt == m_stacks.end()) {
        return false;
    }

    const TrackId trackId = stackIt->first;
    IStackManager::Stack& stack = stackIt->second;
    const auto it = std::find(stack.begin(), stack.end(), oldState);
    const auto index = it - stack.begin();
    stack[index] = newState;

    return true;
}

bool StackManager::reorder(const RealtimeEffectStatePtr& state, int newIndex)
{
    const auto stackIt = findStack(state);
    if (stackIt == m_stacks.end()) {
        return false;
    }
    const TrackId trackId = stackIt->first;
    IStackManager::Stack& stack = stackIt->second;
    if (newIndex >= static_cast<int>(stack.size())) {
        return false;
    }
    const auto it = std::find(stack.begin(), stack.end(), state);
    const auto oldIndex = it - stack.begin();
    if (oldIndex == newIndex) {
        return false;
    }

    stack.erase(it);
    stack.insert(stack.begin() + newIndex, state);

    return true;
}

bool StackManager::doRemove(const RealtimeEffectStatePtr& state)
{
    const auto stackIt = findStack(state);
    if (stackIt == m_stacks.end()) {
        return false;
    }
    const TrackId trackId = stackIt->first;
    IStackManager::Stack& stack = stackIt->second;
    const auto it = std::find(stack.begin(), stack.end(), state);
    stack.erase(it);
    return true;
}

std::optional<TrackId> StackManager::trackId(const RealtimeEffectStatePtr& state) const
{
    const auto stackIt = findStack(state);
    if (stackIt == m_stacks.end()) {
        return {};
    }
    return stackIt->first;
}

std::optional<int> StackManager::effectIndex(const RealtimeEffectStatePtr& state) const
{
    const auto stackIt = findStack(state);
    if (stackIt == m_stacks.end()) {
        return {};
    }
    const TrackId trackId = stackIt->first;
    const IStackManager::Stack& stack = stackIt->second;
    const auto it = std::find(stack.begin(), stack.end(), state);
    return it != stack.end() ? std::make_optional(it - stack.begin()) : std::nullopt;
}

IStackManager::TrackStacks::const_iterator StackManager::findStack(const RealtimeEffectStatePtr& state) const
{
    return std::find_if(m_stacks.begin(), m_stacks.end(), [state](const auto& pair) {
        const IStackManager::Stack& stack = pair.second;
        return std::find(stack.begin(), stack.end(), state) != stack.end();
    });
}

IStackManager::TrackStacks::iterator StackManager::findStack(const RealtimeEffectStatePtr& state)
{
    return std::find_if(m_stacks.begin(), m_stacks.end(), [state](const auto& pair) {
        const IStackManager::Stack& stack = pair.second;
        return std::find(stack.begin(), stack.end(), state) != stack.end();
    });
}

void StackManagerFacade::init()
{
    globalContext()->currentProjectChanged().onNotify(this, [this] {
        if (auto manager = stackManager()) {
            manager->setRealtimeEffectStackChanged(m_realtimeEffectStackChanged);
        }
    });
}

void StackManagerFacade::insert(TrackId trackId, EffectChainLinkIndex index, const RealtimeEffectStatePtr& state)
{
    const auto manager = stackManager();
    IF_ASSERT_FAILED(manager) {
        return;
    }
    if (manager->insert(trackId, index, state)) {
        m_realtimeEffectStackChanged.send(trackId);
    }
}

void StackManagerFacade::remove(const RealtimeEffectStatePtr& state)
{
    const auto manager = stackManager();
    IF_ASSERT_FAILED(manager) {
        return;
    }
    const std::optional<TrackId> trackId = manager->trackId(state);
    if (manager->remove(state)) {
        m_realtimeEffectStackChanged.send(*trackId);
    }
}

void StackManagerFacade::remove(TrackId trackId)
{
    const auto manager = stackManager();
    IF_ASSERT_FAILED(manager) {
        return;
    }
    if (manager->remove(trackId)) {
        m_realtimeEffectStackChanged.send(trackId);
    }
}

void StackManagerFacade::replace(const RealtimeEffectStatePtr& oldState, const RealtimeEffectStatePtr& newState)
{
    const auto manager = stackManager();
    IF_ASSERT_FAILED(manager) {
        return;
    }
    const std::optional<TrackId> trackId = manager->trackId(oldState);
    if (manager->replace(oldState, newState)) {
        m_realtimeEffectStackChanged.send(*trackId);
    }
}

void StackManagerFacade::reorder(const RealtimeEffectStatePtr& state, int newIndex)
{
    const auto manager = stackManager();
    IF_ASSERT_FAILED(manager) {
        return;
    }
    const std::optional<TrackId> trackId = manager->trackId(state);
    const auto up = manager->effectIndex(state) < newIndex;
    if (manager->reorder(state, newIndex)) {
        m_realtimeEffectStackChanged.send(*trackId);
    }
}

std::optional<TrackId> StackManagerFacade::trackId(const RealtimeEffectStatePtr& state) const
{
    if (auto manager = stackManager()) {
        return manager->trackId(state);
    }
    return {};
}

std::optional<int> StackManagerFacade::effectIndex(const RealtimeEffectStatePtr& state) const
{
    if (auto manager = stackManager()) {
        return manager->effectIndex(state);
    }
    return {};
}

StackManager* StackManagerFacade::stackManager() const
{
    const auto project = globalContext()->currentProject();
    return project ? &StackManager::Get(*reinterpret_cast<au3::Au3Project*>(project->au3ProjectPtr())) : nullptr;
}
}
