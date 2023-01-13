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
#include <optional>
#include <unordered_map>
#include <vector>

#include "ClientData.h"
#include "Observer.h"
#include "PluginProvider.h" // for PluginID
#include "RealtimeEffectList.h"

class EffectInstance;

namespace RealtimeEffects {
   class InitializationScope;
   class ProcessingScope;
}

///Posted when effect is being added or removed to/from track or project
struct RealtimeEffectManagerMessage
{
   enum class Type
   {
      EffectAdded,
      EffectReplaced,
      EffectRemoved
   };
   Type type;
   std::shared_ptr<Track> track; ///< null, if changes happened in the project scope
};

class REALTIME_EFFECTS_API RealtimeEffectManager final :
   public ClientData::Base,
   public Observer::Publisher<RealtimeEffectManagerMessage>
{
public:
   using Latency = std::chrono::microseconds;

   RealtimeEffectManager(AudacityProject &project);
   ~RealtimeEffectManager();

   static RealtimeEffectManager & Get(AudacityProject &project);
   static const RealtimeEffectManager & Get(const AudacityProject &project);

   // Realtime effect processing

   //! To be called only from main thread
   bool IsActive() const noexcept;
//   Latency GetLatency() const;

   //! Main thread appends a global or per-track effect
   /*!
    @param pScope if realtime is active but scope is absent, there is no effect
    @param pTrack if null, then state is added to the global list
    @param id identifies the effect
    @return if null, the given id was not found

    @post result: `!result || result->GetEffect() != nullptr`
    */
   std::shared_ptr<RealtimeEffectState> AddState(
      RealtimeEffects::InitializationScope *pScope, Track *pTrack,
      const PluginID & id);

   //! Main thread replaces a global or per-track effect
   /*!
    @param pScope if realtime is active but scope is absent, there is no effect
    @param pTrack if null, then state is added to the global list
    @param index position in the list to replace; no effect if out of range
    @param id identifies the effect
    @return if null, the given id was not found and the old state remains

    @post result: `!result || result->GetEffect() != nullptr`
    */
   std::shared_ptr<RealtimeEffectState> ReplaceState(
      RealtimeEffects::InitializationScope *pScope, Track *pTrack,
      size_t index, const PluginID & id);

   //! Main thread removes a global or per-track effect
   /*!
    @param pScope if realtime is active but scope is absent, there is no effect
    @param pTrack if null, then state is added to the global list
    @param state the state to be removed
    */
   /*! No effect if realtime is active but scope is not supplied */
   void RemoveState(RealtimeEffects::InitializationScope *pScope,
      Track *pTrack, std::shared_ptr<RealtimeEffectState> pState);

   //! Report the position of a state in the global or a per-track list
   std::optional<size_t> FindState(
      Track *pTrack, const std::shared_ptr<RealtimeEffectState> &pState) const;

   bool GetSuspended() const
      { return mSuspended.load(std::memory_order_relaxed); }

   //! To be called only from main thread
   /*!
    Each time a processing scope starts in the audio thread, suspension state
    is tested, and plug-in instances may need to do things in response to the
    transitions.  Playback of samples may continue but with effects switched
    off in suspended state.
    */
   void SetSuspended(bool value)
      { mSuspended.store(value, std::memory_order_relaxed); }

private:
   friend RealtimeEffects::InitializationScope;

   std::shared_ptr<RealtimeEffectState>
   MakeNewState(RealtimeEffects::InitializationScope *pScope,
      Track *pLeader, const PluginID &id);

   //! Main thread begins to define a set of tracks for playback
   void Initialize(RealtimeEffects::InitializationScope &scope,
      double sampleRate);
   //! Main thread adds one track (passing the first of one or more
   //! channels), still before playback
   void AddTrack(RealtimeEffects::InitializationScope &scope,
      const Track &track, unsigned chans, float rate);
   //! Main thread cleans up after playback
   void Finalize() noexcept;

   friend RealtimeEffects::ProcessingScope;
   struct REALTIME_EFFECTS_API AllListsLock {
      RealtimeEffectManager *mpManager{};
      AllListsLock(RealtimeEffectManager *pManager = nullptr);
      AllListsLock(AllListsLock &&other);
      AllListsLock& operator= (AllListsLock &&other);
      void Reset();
      ~AllListsLock() { Reset(); }
   };

   void ProcessStart(bool suspended);
   /*! @copydoc ProcessScope::Process */
   size_t Process(bool suspended, const Track &track,
      float *const *buffers, float *const *scratch, float *dummy,
      unsigned nBuffers, size_t numSamples);
   void ProcessEnd(bool suspended) noexcept;

   RealtimeEffectManager(const RealtimeEffectManager&) = delete;
   RealtimeEffectManager &operator=(const RealtimeEffectManager&) = delete;

   // Type that state visitor functions would have for out-of-line definition
   // of VisitGroup and VisitAll
   // using StateVisitor =
      // std::function<void(RealtimeEffectState &state, bool listIsActive)> ;

   //! Visit the per-project states first, then states for leader if not null
   template<typename StateVisitor>
   void VisitGroup(Track &leader, const StateVisitor &func)
   {
      // Call the function for each effect on the master list
      RealtimeEffectList::Get(mProject).Visit(func);

      // Call the function for each effect on the track list
      RealtimeEffectList::Get(leader).Visit(func);
   }

   template<typename StateVisitor>
   void VisitGroup(const Track &leader, const StateVisitor &func)
   {
      // Call the function for each effect on the master list
      RealtimeEffectList::Get(mProject).Visit(func);

      // Call the function for each effect on the track list
      RealtimeEffectList::Get(leader).Visit(func);
   }

   //! Visit the per-project states first, then all tracks from AddTrack
   /*! Tracks are visited in unspecified order */
   template<typename StateVisitor>
   void VisitAll(const StateVisitor &func)
   {
      // Call the function for each effect on the master list
      RealtimeEffectList::Get(mProject).Visit(func);

      // And all track lists
      for (auto leader : mGroupLeaders)
         RealtimeEffectList::Get(*leader).Visit(func);
   }

   AudacityProject &mProject;
   Latency mLatency{ 0 };

   std::atomic<bool> mSuspended{ true };

   bool mActive{ false };

   // This member is mutated only by Initialize(), AddTrack(), Finalize()
   // which are to be called only while there is no playback
   std::vector<const Track *> mGroupLeaders; //!< all are non-null

   std::unordered_map<const Track *, double> mRates;
};

namespace RealtimeEffects {
//! Brackets processing setup and cleanup in the main thread
class InitializationScope {
public:
   InitializationScope() {}
   explicit InitializationScope(
      std::weak_ptr<AudacityProject> wProject, double sampleRate,
      unsigned numPlaybackChannels
   )  : mSampleRate{ sampleRate }
      , mwProject{ move(wProject) }
      , mNumPlaybackChannels{ numPlaybackChannels }
   {
      if (auto pProject = mwProject.lock())
         RealtimeEffectManager::Get(*pProject).Initialize(*this, sampleRate);
   }
   InitializationScope( InitializationScope &&other ) = default;
   InitializationScope& operator=( InitializationScope &&other ) = default;
   ~InitializationScope()
   {
      if (auto pProject = mwProject.lock())
         RealtimeEffectManager::Get(*pProject).Finalize();
   }

   void AddTrack(const Track &track, unsigned chans, float rate)
   {
      if (auto pProject = mwProject.lock())
         RealtimeEffectManager::Get(*pProject)
            .AddTrack(*this, track, chans, rate);
   }

   std::vector<std::shared_ptr<EffectInstance>> mInstances;
   double mSampleRate;
   unsigned mNumPlaybackChannels;

private:
   std::weak_ptr<AudacityProject> mwProject;
};

//! Brackets one block of processing in one thread
class ProcessingScope {
public:
   ProcessingScope()
   {
      if (auto pProject = mwProject.lock()) {
         auto &manager = RealtimeEffectManager::Get(*pProject);
         mLocks = { &manager };
         mSuspended = manager.GetSuspended();
      }
   }
   //! Require a prior InializationScope to ensure correct nesting
   explicit ProcessingScope(InitializationScope &,
      std::weak_ptr<AudacityProject> wProject)
      : mwProject{ move(wProject) }
   {
      if (auto pProject = mwProject.lock())
         RealtimeEffectManager::Get(*pProject).ProcessStart(mSuspended);
   }
   ProcessingScope( ProcessingScope &&other ) = default;
   ProcessingScope& operator=( ProcessingScope &&other ) = default;
   ~ProcessingScope()
   {
      if (auto pProject = mwProject.lock())
         RealtimeEffectManager::Get(*pProject).ProcessEnd(mSuspended);
   }

   //! @return how many samples to discard for latency
   size_t Process(const Track &track,
      float *const *buffers,
      float *const *scratch,
      float *dummy,
      unsigned nBuffers, //!< how many buffers; equal number of scratches
      size_t numSamples //!< length of each buffer
   )
   {
      if (auto pProject = mwProject.lock())
         return RealtimeEffectManager::Get(*pProject)
            .Process(mSuspended, track, buffers, scratch, dummy,
               nBuffers, numSamples);
      else
         return 0; // consider them trivially processed
   }

private:
   RealtimeEffectManager::AllListsLock mLocks;
   std::weak_ptr<AudacityProject> mwProject;
   bool mSuspended{};
};
}

#endif
