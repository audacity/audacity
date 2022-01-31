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
#include <vector>

#include "ClientData.h"

class AudacityProject;
class EffectProcessor;
class RealtimeEffectState;

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
   bool RealtimeIsActive() const noexcept;
   void RealtimeInitialize(double rate);
   void RealtimeAddProcessor(int group, unsigned chans, float rate);
   void RealtimeFinalize();
   void RealtimeSuspend();
   void RealtimeResume() noexcept;
   Latency GetRealtimeLatency() const;

   //! Object whose lifetime encompasses one suspension of processing in one thread
   class SuspensionScope {
   public:
      explicit SuspensionScope(AudacityProject *pProject)
         : mpProject{ pProject }
      {
         if (mpProject)
            Get(*mpProject).RealtimeSuspend();
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
            Get(*mpProject).RealtimeResume();
      }

   private:
      AudacityProject *mpProject = nullptr;
   };

   //! Object whose lifetime encompasses one block of processing in one thread
   class ProcessScope {
   public:
      explicit ProcessScope(AudacityProject *pProject)
         : mpProject{ pProject }
      {
         if (mpProject)
            Get(*mpProject).RealtimeProcessStart();
      }
      ProcessScope( ProcessScope &&other )
         : mpProject{ other.mpProject }
      {
         other.mpProject = nullptr;
      }
      ProcessScope& operator=( ProcessScope &&other )
      {
         auto pProject = other.mpProject;
         other.mpProject = nullptr;
         mpProject = pProject;
         return *this;
      }
      ~ProcessScope()
      {
         if (mpProject)
            Get(*mpProject).RealtimeProcessEnd();
      }

      size_t Process( int group,
         unsigned chans, float **buffers, size_t numSamples)
      {
         if (mpProject)
            return Get(*mpProject)
               .RealtimeProcess(group, chans, buffers, numSamples);
         else
            return numSamples; // consider them trivially processed
      }

   private:
      AudacityProject *mpProject = nullptr;
   };

private:
   void RealtimeProcessStart();
   size_t RealtimeProcess(int group, unsigned chans, float **buffers, size_t numSamples);
   void RealtimeProcessEnd() noexcept;

   RealtimeEffectManager(const RealtimeEffectManager&) = delete;
   RealtimeEffectManager &operator=(const RealtimeEffectManager&) = delete;

   AudacityProject &mProject;

   std::mutex mLock;
   std::vector< std::unique_ptr<RealtimeEffectState> > mStates;
   Latency mLatency{ 0 };
   std::atomic<bool> mSuspended{ true };
   std::atomic<bool> mActive{ false };
   std::vector<unsigned> mRealtimeChans;
   std::vector<double> mRealtimeRates;
};

#endif
