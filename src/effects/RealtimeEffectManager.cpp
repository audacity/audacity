/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 RealtimeEffectManager.cpp
 
 Paul Licameli split from EffectManager.cpp
 
 **********************************************************************/


#include "RealtimeEffectManager.h"
#include "RealtimeEffectList.h"
#include "RealtimeEffectState.h"

#include "EffectInterface.h"
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

bool RealtimeEffectManager::IsSuspended() const noexcept
{
   return mSuspended;
}

void RealtimeEffectManager::Initialize(double rate)
{
   // The audio thread should not be running yet, but protect anyway
   SuspensionScope scope{ &mProject };

   // (Re)Set processor parameters
   mChans.clear();
   mRates.clear();
   mGroupLeaders.clear();

   // RealtimeAdd/RemoveEffect() needs to know when we're active so it can
   // initialize newly added effects
   mActive = true;

   // Tell each effect of the master list to get ready for action
   VisitGroup(nullptr, [rate](RealtimeEffectState &state, bool){
      state.Initialize(rate);
   });
}

void RealtimeEffectManager::AddTrack(Track *track, unsigned chans, float rate)
{
   auto leader = *track->GetOwner()->FindLeader(track);
   mGroupLeaders.push_back(leader);
   mChans.insert({leader, chans});
   mRates.insert({leader, rate});

   VisitGroup(leader,
      [&](RealtimeEffectState & state, bool) {
         state.Initialize(rate);
         state.AddTrack(leader, chans, rate);
      }
   );
}

void RealtimeEffectManager::Finalize()
{
   // Make sure nothing is going on
   Suspend();

   // It is now safe to clean up
   mLatency = std::chrono::microseconds(0);

   VisitAll([](auto &state, bool){ state.Finalize(); });

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
   VisitGroup(nullptr, [](RealtimeEffectState &state, bool){
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
   VisitGroup(nullptr, [](RealtimeEffectState &state, bool){
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
      VisitGroup(nullptr, [](RealtimeEffectState &state, bool bypassed){
         if (!bypassed)
            state.ProcessStart();
      });
   }
}

//
// This will be called in a different thread than the main GUI thread.
//
size_t RealtimeEffectManager::Process(Track *track,
                                      float **buffers,
                                      size_t numSamples)
{
   // Protect...
   std::lock_guard<std::mutex> guard(mLock);

   // Can be suspended because of the audio stream being paused or because effects
   // have been suspended, so allow the samples to pass as-is.
   if (mSuspended)
      return numSamples;

   auto chans = mChans[track];

   // Remember when we started so we can calculate the amount of latency we
   // are introducing
   auto start = std::chrono::steady_clock::now();

   // Allocate the in and out buffer arrays
   float **ibuf = (float **) alloca(chans * sizeof(float *));
   float **obuf = (float **) alloca(chans * sizeof(float *));

   // And populate the input with the buffers we've been given while allocating
   // NEW output buffers
   for (unsigned int i = 0; i < chans; i++)
   {
      ibuf[i] = buffers[i];
      obuf[i] = (float *) alloca(numSamples * sizeof(float));
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

         state.Process(track, chans, ibuf, obuf, numSamples);
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
      VisitGroup(nullptr, [](RealtimeEffectState &state, bool bypassed){
         if (!bypassed)
            state.ProcessEnd();
      });
   }
}

void RealtimeEffectManager::VisitGroup(Track *leader, StateVisitor func)
{
   // Call the function for each effect on the master list
   RealtimeEffectList::Get(mProject).Visit(func);

   // Call the function for each effect on the track list
   if (leader)
     RealtimeEffectList::Get(*leader).Visit(func);
}

void RealtimeEffectManager::VisitAll(StateVisitor func)
{
   // Call the function for each effect on the master list
   RealtimeEffectList::Get(mProject).Visit(func);

   // And all track lists
   for (auto leader : mGroupLeaders)
      RealtimeEffectList::Get(*leader).Visit(func);
}

auto RealtimeEffectManager::GetLatency() const -> Latency
{
   return mLatency;
}
