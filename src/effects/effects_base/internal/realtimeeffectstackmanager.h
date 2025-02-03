/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "irealtimeeffectstackmanager.h"
#include "au3wrap/au3types.h"
#include "context/iglobalcontext.h"
#include "libraries/lib-utility/Observer.h"
#include "libraries/lib-registries/ClientData.h"
#include "async/channel.h"
#include "modularity/ioc.h"
#include <functional>
#include <stack>

namespace au::effects {
class StackManager : public ClientData::Base, public muse::Injectable
{
    muse::Inject<IStackManager> facade;

public:
    static StackManager& Get(AudacityProject& project);

    bool insert(TrackId trackId, EffectChainLinkIndex index, const RealtimeEffectStatePtr& state);
    bool remove(const RealtimeEffectStatePtr& state);
    std::optional<IStackManager::Stack> remove(TrackId trackId);
    bool replace(const RealtimeEffectStatePtr& oldState, const RealtimeEffectStatePtr& newState);
    bool reorder(const RealtimeEffectStatePtr& state, int newIndex);

    std::optional<TrackId> trackId(const RealtimeEffectStatePtr& state) const;
    std::optional<int> effectIndex(const RealtimeEffectStatePtr& state) const;
    std::optional<IStackManager::Stack> effectStack(TrackId trackId) const
    {
        return m_stacks.count(trackId) ? std::make_optional(m_stacks.at(trackId)) : std::nullopt;
    }

    void setRealtimeEffectStackChanged(muse::async::Channel<TrackId> channel)
    {
        m_realtimeEffectStackChanged = std::move(channel);
    }

private:
    bool doRemove(const RealtimeEffectStatePtr& state);

    IStackManager::TrackStacks::const_iterator findStack(const RealtimeEffectStatePtr& state) const;
    IStackManager::TrackStacks::iterator findStack(const RealtimeEffectStatePtr& state);

    // Models how effects are stacked on a track, i.e., the order of the effects in the vectors matters.
    IStackManager::TrackStacks m_stacks;
    muse::async::Channel<TrackId> m_realtimeEffectStackChanged;

    friend class EffectStackRestorer;
};

class StackManagerFacade : public IStackManager, public muse::Injectable, public muse::async::Asyncable
{
    muse::Inject<context::IGlobalContext> globalContext;

public:
    void init();

    void insert(TrackId trackId, EffectChainLinkIndex index, const RealtimeEffectStatePtr& state) override;
    void remove(const RealtimeEffectStatePtr& state) override;
    void remove(TrackId trackId) override;
    void replace(const RealtimeEffectStatePtr& oldState, const RealtimeEffectStatePtr& newState) override;
    void reorder(const RealtimeEffectStatePtr& state, int newIndex) override;

    muse::async::Channel<TrackId> realtimeEffectStackChanged() const override
    {
        return m_realtimeEffectStackChanged;
    }

    std::optional<Stack> effectStack(TrackId trackId) const override { return stackManager()->effectStack(trackId); }

    std::optional<TrackId> trackId(const RealtimeEffectStatePtr& state) const override;
    std::optional<int> effectIndex(const RealtimeEffectStatePtr& state) const override;

private:
    StackManager* stackManager() const;

    muse::async::Channel<TrackId> m_realtimeEffectStackChanged;
};
}
