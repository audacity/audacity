/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 RealtimeEffectManager.cpp
 
 Paul Licameli split from EffectManager.cpp
 
 **********************************************************************/


#include "RealtimeEffectManager.h"
#include "RealtimeEffectList.h"
#include "RealtimeEffectState.h"

#include <memory>
#include "Project.h"
#include "Track.h"

#include <atomic>
#include <wx/time.h>

static const AttachedProjectObjects::RegisteredFactory manager
{
   [](AudacityProject &project)
   {
      return std::make_shared<RealtimeEffectManager>(project);
   }
};

RealtimeEffectManager &RealtimeEffectManager::Get(AudacityProject &project)
{
   return project.AttachedObjects::Get<RealtimeEffectManager&>(manager);
}

const RealtimeEffectManager &RealtimeEffectManager::Get(const AudacityProject &project)
{
   return Get(const_cast<AudacityProject &>(project));
}

RealtimeEffectManager::RealtimeEffectManager(AudacityProject &project)
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

void RealtimeEffectManager::Initialize(double rate)
{
   // Remember the rate
   mRate = rate;

   // (Re)Set processor parameters
   mChans.clear();
   mRates.clear();
   mGroupLeaders.clear();

   // RealtimeAdd/RemoveEffect() needs to know when we're active so it can
   // initialize newly added effects
   mActive = true;

   // Tell each state to get ready for action
   VisitAll([rate](RealtimeEffectState &state, bool){
      state.Initialize(rate);
   });

   // Leave suspended state
   Resume();
}

void RealtimeEffectManager::AddTrack(Track &track, unsigned chans, float rate)
{
   auto leader = *track.GetOwner()->FindLeader(&track);
   // This should never return a null
   wxASSERT(leader);
   mGroupLeaders.push_back(leader);
   mChans.insert({leader, chans});
   mRates.insert({leader, rate});

   VisitGroup(*leader,
      [&](RealtimeEffectState & state, bool) {
         state.AddTrack(*leader, chans, rate);
      }
   );
}

void RealtimeEffectManager::Finalize() noexcept
{
   // Reenter suspended state
   Suspend();

   // Assume it is now safe to clean up
   mLatency = std::chrono::microseconds(0);

   VisitAll([](RealtimeEffectState &state, bool){ state.Finalize(); });

   // Reset processor parameters
   mGroupLeaders.clear();
   mChans.clear();
   mRates.clear();

   // No longer active
   mActive = false;
}

void RealtimeEffectManager::Suspend()
{
   // Protect...
   std::lock_guard<std::mutex> guard(mLock);

   // Already suspended...bail
   if (mSuspended)
      return;

   // Show that we aren't going to be doing anything
   mSuspended = true;

   // And make sure the effects don't either
   VisitAll([](RealtimeEffectState &state, bool){
      state.Suspend();
   });
}

void RealtimeEffectManager::Resume() noexcept
{
   // Protect...
   std::lock_guard<std::mutex> guard(mLock);

   // Already running...bail
   if (!mSuspended)
      return;

   // Tell the effects to get ready for more action
   VisitAll([](RealtimeEffectState &state, bool){
      state.Resume();
   });

   // And we should too
   mSuspended = false;
}

//
// This will be called in a different thread than the main GUI thread.
//
void RealtimeEffectManager::ProcessStart()
{
   // Protect...
   std::lock_guard<std::mutex> guard(mLock);

   // Can be suspended because of the audio stream being paused or because effects
   // have been suspended.
   if (!mSuspended)
   {
      VisitAll([](RealtimeEffectState &state, bool bypassed){
         if (!bypassed)
            state.ProcessStart();
      });
   }
}

//
// This will be called in a different thread than the main GUI thread.
//
size_t RealtimeEffectManager::Process(Track &track,
   float *const *buffers, float *const *scratch,
   size_t numSamples)
{
   // Protect...
   std::lock_guard<std::mutex> guard(mLock);

   // Can be suspended because of the audio stream being paused or because effects
   // have been suspended, so allow the samples to pass as-is.
   if (mSuspended)
      return numSamples;

   auto chans = mChans[&track];

   // Remember when we started so we can calculate the amount of latency we
   // are introducing
   auto start = std::chrono::steady_clock::now();

   // Allocate the in and out buffer arrays
   const auto ibuf =
      static_cast<float **>(alloca(chans * sizeof(float *)));
   const auto obuf =
      static_cast<float **>(alloca(chans * sizeof(float *)));

   // And populate the input with the buffers we've been given while allocating
   // NEW output buffers
   for (unsigned int i = 0; i < chans; i++)
   {
      ibuf[i] = buffers[i];
      obuf[i] = scratch[i];
   }

   // Now call each effect in the chain while swapping buffer pointers to feed the
   // output of one effect as the input to the next effect
   // Tracks how many processors were called
   size_t called = 0;
   VisitGroup(track,
      [&](RealtimeEffectState &state, bool bypassed)
      {
         if (bypassed)
            return;

         state.Process(track, chans, ibuf, obuf, scratch[chans], numSamples);
         for (auto i = 0; i < chans; ++i)
            std::swap(ibuf[i], obuf[i]);
         called++;
      }
   );

   // Once we're done, we might wind up with the last effect storing its results
   // in the temporary buffers.  If that's the case, we need to copy it over to
   // the caller's buffers.  This happens when the number of effects processed
   // is odd.
   if (called & 1)
      for (unsigned int i = 0; i < chans; i++)
         memcpy(buffers[i], ibuf[i], numSamples * sizeof(float));

   // Remember the latency
   auto end = std::chrono::steady_clock::now();
   mLatency = std::chrono::duration_cast<std::chrono::microseconds>(end - start);

   //
   // This is wrong...needs to handle tails
   //
   return numSamples;
}

//
// This will be called in a different thread than the main GUI thread.
//
void RealtimeEffectManager::ProcessEnd() noexcept
{
   // Protect...
   std::lock_guard<std::mutex> guard(mLock);

   // Can be suspended because of the audio stream being paused or because effects
   // have been suspended.
   if (!mSuspended)
   {
      VisitAll([](RealtimeEffectState &state, bool bypassed){
         if (!bypassed)
            state.ProcessEnd();
      });
   }
}

void RealtimeEffectManager::VisitGroup(Track &leader, StateVisitor func)
{
   // Call the function for each effect on the master list
   RealtimeEffectList::Get(mProject).Visit(func);

   // Call the function for each effect on the track list
   RealtimeEffectList::Get(leader).Visit(func);
}

void RealtimeEffectManager::VisitAll(StateVisitor func)
{
   // Call the function for each effect on the master list
   RealtimeEffectList::Get(mProject).Visit(func);

   // And all track lists
   for (auto leader : mGroupLeaders)
      RealtimeEffectList::Get(*leader).Visit(func);
}

RealtimeEffectState *
RealtimeEffectManager::AddState(
   RealtimeEffects::InitializationScope *pScope,
   Track *pTrack, const PluginID & id)
{
   auto pLeader = pTrack ? *TrackList::Channels(pTrack).begin() : nullptr;
   RealtimeEffectList &states = pLeader
      ? RealtimeEffectList::Get(*pLeader)
      : RealtimeEffectList::Get(mProject);

   std::optional<RealtimeEffects::SuspensionScope> myScope;
   if (mActive) {
      if (pScope)
         myScope.emplace(*pScope, mProject.weak_from_this());
      else
         return nullptr;
   }
   // Protect...
   std::lock_guard<std::mutex> guard(mLock);

   auto pState = states.AddState(id);
   if (!pState)
      return nullptr;
   auto &state = *pState;
   
   if (mActive)
   {
      // Adding a state while playback is in-flight
      state.Initialize(mRate);

      for (auto &leader : mGroupLeaders) {
         // Add all tracks to a per-project state, but add only the same track
         // to a state in the per-track list
         if (pLeader && pLeader != leader)
            continue;

         auto chans = mChans[leader];
         auto rate = mRates[leader];

         state.AddTrack(*leader, chans, rate);
      }
   }

   Publish({
      RealtimeEffectManagerMessage::Type::EffectAdded,
      pLeader ? pLeader->shared_from_this() : nullptr
   });

   return &state;
}

void RealtimeEffectManager::RemoveState(
   RealtimeEffects::InitializationScope *pScope,
   Track *pTrack, RealtimeEffectState &state)
{
   auto pLeader = pTrack ? *TrackList::Channels(pTrack).begin() : nullptr;
   RealtimeEffectList &states = pLeader
      ? RealtimeEffectList::Get(*pLeader)
      : RealtimeEffectList::Get(mProject);

   std::optional<RealtimeEffects::SuspensionScope> myScope;
   if (mActive) {
      if (pScope)
         myScope.emplace(*pScope, mProject.weak_from_this());
      else
         return;
   }
   // Protect...
   std::lock_guard<std::mutex> guard(mLock);

   if (mActive)
      state.Finalize();

   states.RemoveState(state);

   Publish({
      RealtimeEffectManagerMessage::Type::EffectRemoved,
      pLeader ? pLeader->shared_from_this() : nullptr
   });
}

auto RealtimeEffectManager::GetLatency() const -> Latency
{
   return mLatency;
}
