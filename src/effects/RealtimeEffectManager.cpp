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
   // The audio thread should not be running yet, but protect anyway
   SuspensionScope scope{ &mProject };

   // (Re)Set processor parameters
   mChans.clear();
   mRates.clear();

   // RealtimeAdd/RemoveEffect() needs to know when we're active so it can
   // initialize newly added effects
   mActive = true;

   // Tell each effect to get ready for action
   VisitGroup(nullptr, [rate](RealtimeEffectState &state, bool){
      state.Initialize(rate);
   });
}

void RealtimeEffectManager::AddTrack(int group, unsigned chans, float rate)
{
   VisitGroup(nullptr, [&](RealtimeEffectState &state, bool){
      state.AddTrack(group, chans, rate);
   });

   mChans.push_back(chans);
   mRates.push_back(rate);
}

void RealtimeEffectManager::Finalize()
{
   // Make sure nothing is going on
   Suspend();

   // It is now safe to clean up
   mLatency = std::chrono::microseconds(0);

   // Tell each effect to clean up as well
   VisitGroup(nullptr, [](RealtimeEffectState &state, bool){
      state.Finalize();
   });

   // Reset processor parameters
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
size_t RealtimeEffectManager::Process(int group, unsigned chans, float **buffers, size_t numSamples)
{
   // Protect...
   std::lock_guard<std::mutex> guard(mLock);

   // Can be suspended because of the audio stream being paused or because effects
   // have been suspended, so allow the samples to pass as-is.
   if (mSuspended)
      return numSamples;

   // Remember when we started so we can calculate the amount of latency we
   // are introducing
   auto start = std::chrono::steady_clock::now();

   // Allocate the in/out buffer arrays
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
   size_t called = 0;
   VisitGroup(nullptr, [&](RealtimeEffectState &state, bool bypassed){
      if (!bypassed) {
         state.Process(group, chans, ibuf, obuf, numSamples);
         called++;
      }

      for (unsigned int j = 0; j < chans; j++)
      {
         float *temp;
         temp = ibuf[j];
         ibuf[j] = obuf[j];
         obuf[j] = temp;
      }
   });

   // Once we're done, we might wind up with the last effect storing its results
   // in the temporary buffers.  If that's the case, we need to copy it over to
   // the caller's buffers.  This happens when the number of effects processed
   // is odd.
   if (called & 1)
   {
      for (unsigned int i = 0; i < chans; i++)
      {
         memcpy(buffers[i], ibuf[i], numSamples * sizeof(float));
      }
   }

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

void RealtimeEffectManager::VisitGroup(Track *leader,
   std::function<void(RealtimeEffectState &state, bool bypassed)> func)
{
   // Call the function for each effect on the master list
   RealtimeEffectList::Get(mProject).Visit(func);

   // Call the function for each effect on the track list
   if (leader)
     RealtimeEffectList::Get(*leader).Visit(func);
}

auto RealtimeEffectManager::GetLatency() const -> Latency
{
   return mLatency;
}
