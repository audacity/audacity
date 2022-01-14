/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 RealtimeEffectManager.h
 
 Paul Licameli split from EffectManager.h
 
 **********************************************************************/

#ifndef __AUDACITY_REALTIME_EFFECT_MANAGER__
#define __AUDACITY_REALTIME_EFFECT_MANAGER__

#include <atomic>
#include <chrono>
#include <memory>
#include <mutex>
#include <unordered_map>
#include <vector>

#include "ClientData.h"
#include "ModuleInterface.h" // for PluginID

class AudacityProject;
class EffectProcessor;
class RealtimeEffectList;
class RealtimeEffectState;
class Track;

namespace RealtimeEffects {
   class SuspensionScope;
   class ProcessingScope;
}

class AUDACITY_DLL_API RealtimeEffectManager final
   : public ClientData::Base
{
public:
   using Latency = std::chrono::microseconds;
   using EffectArray = std::vector <EffectProcessor*> ;

   RealtimeEffectManager(AudacityProject &project);
   ~RealtimeEffectManager();

   static RealtimeEffectManager & Get(AudacityProject &project);
   static const RealtimeEffectManager & Get(const AudacityProject &project);

   // Realtime effect processing
   bool IsActive() const noexcept;
   //! Main thread begins to define a set of tracks for playback
   void Initialize(double rate);
   //! Main thread adds one track (passing the first of one or more channels)
   void AddTrack(Track *track, unsigned chans, float rate);
   //! Main thread cleans up after playback
   void Finalize();
   void Suspend();
   void Resume() noexcept;
   Latency GetLatency() const;

   //! Main thread appends a global or per-track effect
   /*!
    @param pTrack if null, then state is added to the global list
    @return if null, the given id was not found
    */
   RealtimeEffectState *AddState(Track *pTrack, const PluginID & id);
   //! Main thread safely removes an effect from a list
   void RemoveState(RealtimeEffectList &states, RealtimeEffectState &state);

private:

   friend RealtimeEffects::ProcessingScope;
   void ProcessStart();
   size_t Process(Track *track, float **buffers, size_t numSamples);
   void ProcessEnd() noexcept;

   RealtimeEffectManager(const RealtimeEffectManager&) = delete;
   RealtimeEffectManager &operator=(const RealtimeEffectManager&) = delete;

   using StateVisitor =
      std::function<void(RealtimeEffectState &state, bool bypassed)> ;

   //! Visit the per-project states first, then states for leader if not null
   void VisitGroup(Track *leader, StateVisitor func);

   //! Visit the per-project states first, then all tracks from AddTrack
   /*! Tracks are visited in unspecified order */
   void VisitAll(StateVisitor func);

   AudacityProject &mProject;

   std::mutex mLock;
   Latency mLatency{ 0 };

   double mRate;

   std::atomic<bool> mSuspended{ true };
   std::atomic<bool> mActive{ false };

   std::vector<Track *> mGroupLeaders;
   std::unordered_map<Track *, unsigned> mChans;
   std::unordered_map<Track *, double> mRates;
};

namespace RealtimeEffects {
//! Brackets one suspension of processing in the main thread
class SuspensionScope {
public:
   SuspensionScope() {}
   explicit SuspensionScope(AudacityProject *pProject)
      : mpProject{ pProject }
   {
      if (mpProject)
         RealtimeEffectManager::Get(*mpProject).Suspend();
   }
   SuspensionScope( SuspensionScope &&other )
      : mpProject{ other.mpProject }
   {
      other.mpProject = nullptr;
   }
   SuspensionScope& operator=( SuspensionScope &&other )
   {
      auto pProject = other.mpProject;
      other.mpProject = nullptr;
      mpProject = pProject;
      return *this;
   }
   ~SuspensionScope()
   {
      if (mpProject)
         RealtimeEffectManager::Get(*mpProject).Resume();
   }

private:
   AudacityProject *mpProject = nullptr;
};

//! Brackets one block of processing in one thread
class ProcessingScope {
public:
   ProcessingScope() {}
   explicit ProcessingScope(AudacityProject *pProject)
      : mpProject{ pProject }
   {
      if (mpProject)
         RealtimeEffectManager::Get(*mpProject).ProcessStart();
   }
   ProcessingScope( ProcessingScope &&other )
      : mpProject{ other.mpProject }
   {
      other.mpProject = nullptr;
   }
   ProcessingScope& operator=( ProcessingScope &&other )
   {
      auto pProject = other.mpProject;
      other.mpProject = nullptr;
      mpProject = pProject;
      return *this;
   }
   ~ProcessingScope()
   {
      if (mpProject)
         RealtimeEffectManager::Get(*mpProject).ProcessEnd();
   }

   size_t Process(Track *track, float **buffers, size_t numSamples)
   {
      if (mpProject)
         return RealtimeEffectManager::Get(*mpProject)
            .Process(track, buffers, numSamples);
      else
         return numSamples; // consider them trivially processed
   }

private:
   AudacityProject *mpProject = nullptr;
};
}

#endif
