/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 RealtimeEffectManager.h
 
 Paul Licameli split from EffectManager.h
 
 **********************************************************************/

#ifndef __AUDACITY_REALTIME_EFFECT_MANAGER__
#define __AUDACITY_REALTIME_EFFECT_MANAGER__

#include <memory>
#include <vector>
#include <wx/thread.h>

class Effect;
using EffectArray = std::vector <Effect*> ;

class AUDACITY_DLL_API RealtimeEffectManager final
{
public:

   /** Get the singleton instance of the RealtimeEffectManager. **/
   static RealtimeEffectManager & Get();

   // Realtime effect processing
   bool RealtimeIsActive();
   bool RealtimeIsSuspended();
   void RealtimeAddEffect(Effect *effect);
   void RealtimeRemoveEffect(Effect *effect);
   void RealtimeSetEffects(const EffectArray & mActive);
   void RealtimeInitialize(double rate);
   void RealtimeAddProcessor(int group, unsigned chans, float rate);
   void RealtimeFinalize();
   void RealtimeSuspend();
   void RealtimeResume();
   void RealtimeProcessStart();
   size_t RealtimeProcess(int group, unsigned chans, float **buffers, size_t numSamples);
   void RealtimeProcessEnd();
   int GetRealtimeLatency();

private:
   RealtimeEffectManager();
   ~RealtimeEffectManager();

   wxCriticalSection mRealtimeLock;
   EffectArray mRealtimeEffects;
   int mRealtimeLatency;
   bool mRealtimeSuspended;
   bool mRealtimeActive;
   std::vector<unsigned> mRealtimeChans;
   std::vector<double> mRealtimeRates;
};

#endif
