/**********************************************************************

 Audacity: A Digital Audio Editor

 RealtimeEffectManager.cpp

 Paul Licameli split from EffectManager.cpp

 **********************************************************************/
#include "RealtimeEffectManager.h"
#include "RealtimeEffectState.h"
#include "Channel.h"

#include <memory>
#include "Project.h"

#include <atomic>
#include <wx/time.h>

static const AttachedProjectObjects::RegisteredFactory manager
{
    [](AudacityProject& project)
    {
        return std::make_shared<RealtimeEffectManager>(project);
    }
};

RealtimeEffectManager& RealtimeEffectManager::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<RealtimeEffectManager&>(manager);
}

const RealtimeEffectManager&
RealtimeEffectManager::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

RealtimeEffectManager::RealtimeEffectManager(AudacityProject& project)
    : mProject(project)
{
}

RealtimeEffectManager::~RealtimeEffectManager()
{
}

bool RealtimeEffectManager::IsActive() const noexcept
{
    return mActive;
}

void RealtimeEffectManager::Initialize(
    RealtimeEffects::InitializationScope& scope,
    unsigned numPlaybackChannels,
    double sampleRate)
{
    // (Re)Set processor parameters
    mRates.clear();
    mGroups.clear();

    // RealtimeAdd/RemoveEffect() needs to know when we're active so it can
    // initialize newly added effects
    mActive = true;

    // Tell each state to get ready for action
    VisitAll([&scope, sampleRate](RealtimeEffectState& state, bool) {
        scope.mInstances.push_back(state.Initialize(sampleRate));
    });

    // Leave suspended state
    SetSuspended(false);

    VisitGroup(MasterGroup, [&](RealtimeEffectState& state, bool) {
        scope.mInstances.push_back(state.AddGroup(MasterGroup, numPlaybackChannels, sampleRate));
    });
}

void RealtimeEffectManager::AddGroup(
    RealtimeEffects::InitializationScope& scope,
    const ChannelGroup& group, unsigned chans, float rate)
{
    mGroups.push_back(&group);
    mRates.insert({ &group, rate });

    VisitGroup(&group,
               [&](RealtimeEffectState& state, bool) {
        scope.mInstances.push_back(state.AddGroup(&group, chans, rate));
    }
               );
}

void RealtimeEffectManager::Finalize() noexcept
{
    // Reenter suspended state
    SetSuspended(true);

    VisitAll([](RealtimeEffectState& state, bool){ state.Finalize(); });

    // Reset processor parameters
    mGroups.clear();
    mRates.clear();

    // No longer active
    mActive = false;
}

//
// This will be called in a different thread than the main GUI thread.
//
void RealtimeEffectManager::ProcessStart(bool suspended)
{
    // Can be suspended because of the audio stream being paused or because
    // effects have been suspended.
    VisitAll([suspended](RealtimeEffectState& state, bool listIsActive){
        state.ProcessStart(!suspended && listIsActive);
    });
}

// This will be called in a thread other than the main GUI thread.
//
size_t RealtimeEffectManager::Process(bool suspended,
                                      const ChannelGroup* group,
                                      float* const* buffers, float* const* scratch, float* const dummy,
                                      unsigned nBuffers, size_t numSamples)
{
    // Can be suspended because of the audio stream being paused or because
    // effects have been suspended, so allow the samples to pass as-is.
    if (suspended) {
        return 0;
    }

    // Allocate the in and out buffer arrays
    const auto ibuf
        =static_cast<float**>(alloca(nBuffers * sizeof(float*)));
    const auto obuf
        =static_cast<float**>(alloca(nBuffers * sizeof(float*)));

    // And populate the input with the buffers we've been given while allocating
    // NEW output buffers
    for (unsigned int i = 0; i < nBuffers; i++) {
        ibuf[i] = buffers[i];
        obuf[i] = scratch[i];
    }

    // Now call each effect in the chain while swapping buffer pointers to feed
    // the output of one effect as the input to the next effect
    // Tracks how many processors were called
    size_t called = 0;
    size_t discardable = 0;
    VisitGroup(group,
               [&](RealtimeEffectState& state, bool)
    {
        discardable
            +=state.Process(group, nBuffers, ibuf, obuf, dummy, numSamples);
        for (auto i = 0; i < nBuffers; ++i) {
            std::swap(ibuf[i], obuf[i]);
        }
        called++;
    }
               );

    // Once we're done, we might wind up with the last effect storing its results
    // in the temporary buffers.  If that's the case, we need to copy it over to
    // the caller's buffers.  This happens when the number of effects processed
    // is odd.
    if (called & 1) {
        for (unsigned int i = 0; i < nBuffers; i++) {
            memcpy(buffers[i], ibuf[i], numSamples * sizeof(float));
        }
    }

    //
    // This is wrong...needs to handle tails
    //
    return discardable;
}

//
// This will be called in a different thread than the main GUI thread.
//
void RealtimeEffectManager::ProcessEnd(bool suspended) noexcept
{
    // Can be suspended because of the audio stream being paused or because
    // effects have been suspended.
    VisitAll([suspended](RealtimeEffectState& state, bool){
        state.ProcessEnd();
    });
}

std::shared_ptr<RealtimeEffectState>
RealtimeEffectManager::MakeNewState(
    RealtimeEffects::InitializationScope* pScope,
    ChannelGroup* pGroup, const PluginID& id)
{
    if (!pScope && mActive) {
        return nullptr;
    }
    auto pNewState = RealtimeEffectState::make_shared(id);
    auto& state = *pNewState;
    if (pScope && mActive) {
        // Adding a state while playback is in-flight
        auto pInstance = state.Initialize(pScope->mSampleRate);
        pScope->mInstances.push_back(pInstance);

        if (pGroup == MasterGroup) {
            auto pInstance2 = state.AddGroup(MasterGroup, pScope->mNumPlaybackChannels, pScope->mSampleRate);
            if (pInstance2 != pInstance) {
                pScope->mInstances.push_back(pInstance2);
            }
        } else {
            for (const auto group : mGroups) {
                if (pGroup != group) {
                    continue;
                }
                auto pInstance2
                    =state.AddGroup(group, pScope->mNumPlaybackChannels, mRates[group]);
                if (pInstance2 != pInstance) {
                    pScope->mInstances.push_back(pInstance2);
                }
            }
        }
    }
    return pNewState;
}

namespace {
RealtimeEffectList&
FindStates(AudacityProject& project, ChannelGroup* pGroup)
{
    return pGroup
           ? RealtimeEffectList::Get(*pGroup)
           : RealtimeEffectList::Get(project);
}
}

std::shared_ptr<RealtimeEffectState> RealtimeEffectManager::AddState(
    RealtimeEffects::InitializationScope* pScope,
    ChannelGroup* pGroup, const PluginID& id)
{
    auto& states = FindStates(mProject, pGroup);
    auto pState = MakeNewState(pScope, pGroup, id);
    if (!pState) {
        return nullptr;
    }

    // Only now add the completed state to the list, under a lock guard
    if (!states.AddState(pState)) {
        return nullptr;
    }
    Publish({
        RealtimeEffectManagerMessage::Type::EffectAdded,
        pGroup ? pGroup : MasterGroup
    });
    return pState;
}

std::shared_ptr<RealtimeEffectState> RealtimeEffectManager::ReplaceState(
    RealtimeEffects::InitializationScope* pScope,
    ChannelGroup* pGroup, size_t index, const PluginID& id)
{
    auto& states = FindStates(mProject, pGroup);
    auto pOldState = states.GetStateAt(index);
    if (!pOldState) {
        return nullptr;
    }
    auto pNewState = MakeNewState(pScope, pGroup, id);
    if (!pNewState) {
        return nullptr;
    }

    // Only now swap the completed state into the list, under a lock guard
    if (!states.ReplaceState(index, pNewState)) {
        return nullptr;
    }
    if (mActive) {
        pOldState->Finalize();
    }
    Publish({
        RealtimeEffectManagerMessage::Type::EffectReplaced, pGroup
    });
    return pNewState;
}

void RealtimeEffectManager::RemoveState(
    RealtimeEffects::InitializationScope* pScope,
    ChannelGroup* pGroup,
    const std::shared_ptr<RealtimeEffectState> pState)
{
    auto& states = FindStates(mProject, pGroup);

    // Remove the state from processing (under the lock guard) before finalizing
    states.RemoveState(pState);
    if (mActive) {
        pState->Finalize();
    }
    Publish({
        RealtimeEffectManagerMessage::Type::EffectRemoved,
        pGroup ? pGroup : MasterGroup
    });
}

std::optional<size_t> RealtimeEffectManager::FindState(
    ChannelGroup* pGroup,
    const std::shared_ptr<RealtimeEffectState>& pState) const
{
    auto& states = FindStates(mProject, pGroup);
    return states.FindState(pState);
}
