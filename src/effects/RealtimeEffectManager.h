/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 RealtimeEffectManager.h
 
 Paul Licameli split from EffectManager.h
 
 **********************************************************************/

#ifndef __AUDACITY_REALTIME_EFFECT_MANAGER__
#define __AUDACITY_REALTIME_EFFECT_MANAGER__

#include <chrono>
#include <memory>
#include <mutex>
#include <vector>

class EffectProcessor;
class RealtimeEffectState;

class AUDACITY_DLL_API RealtimeEffectManager final
{
public:
   using Latency = std::chrono::microseconds;
   using EffectArray = std::vector <EffectProcessor*> ;

   /** Get the singleton instance of the RealtimeEffectManager. **/
   static RealtimeEffectManager & Get();

   // Realtime effect processing
   bool RealtimeIsActive();
   bool RealtimeIsSuspended();
   void RealtimeAddEffect(EffectProcessor &effect);
   void RealtimeRemoveEffect(EffectProcessor &effect);
   void RealtimeInitialize(double rate);
   void RealtimeAddProcessor(int group, unsigned chans, float rate);
   void RealtimeFinalize();
   void RealtimeSuspend();
   void RealtimeSuspendOne( EffectProcessor &effect );
   void RealtimeResume() noexcept;
   void RealtimeResumeOne( EffectProcessor &effect );
   Latency GetRealtimeLatency() const;

   //! Object whose lifetime encompasses one suspension of processing in one thread
   class SuspensionScope {
   public:
      SuspensionScope()
      {
         Get().RealtimeSuspend();
      }
      SuspensionScope( SuspensionScope &&other )
      {
         other.mMoved = true;
      }
      SuspensionScope& operator=( SuspensionScope &&other )
      {
         auto moved = other.mMoved;
         other.mMoved = true;
         mMoved = moved;
         return *this;
      }
      ~SuspensionScope()
      {
         if (!mMoved)
            Get().RealtimeResume();
      }

   private:
      bool mMoved{ false };
   };

   //! Object whose lifetime encompasses one block of processing in one thread
   class ProcessScope {
   public:
      ProcessScope()
      {
         Get().RealtimeProcessStart();
      }
      ProcessScope( ProcessScope &&other )
      {
         other.mMoved = true;
      }
      ProcessScope& operator=( ProcessScope &&other )
      {
         auto moved = other.mMoved;
         other.mMoved = true;
         mMoved = moved;
         return *this;
      }
      ~ProcessScope()
      {
         if (!mMoved)
            Get().RealtimeProcessEnd();
      }

      size_t Process( int group,
         unsigned chans, float **buffers, size_t numSamples)
      {
         return Get().RealtimeProcess(group, chans, buffers, numSamples);
      }

   private:
      bool mMoved{ false };
   };

private:
   void RealtimeProcessStart();
   size_t RealtimeProcess(int group, unsigned chans, float **buffers, size_t numSamples);
   void RealtimeProcessEnd() noexcept;

   RealtimeEffectManager();
   ~RealtimeEffectManager();
   RealtimeEffectManager(const RealtimeEffectManager&) = delete;
   RealtimeEffectManager &operator=(const RealtimeEffectManager&) = delete;

   std::mutex mLock;
   std::vector< std::unique_ptr<RealtimeEffectState> > mStates;
   Latency mLatency{ 0 };
   bool mRealtimeSuspended;
   bool mRealtimeActive;
   std::vector<unsigned> mRealtimeChans;
   std::vector<double> mRealtimeRates;
};

#endif
