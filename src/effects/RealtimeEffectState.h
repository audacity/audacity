/**********************************************************************

 Audacity: A Digital Audio Editor

 @file RealtimeEffectState.h

 Paul Licameli split from RealtimeEffectManager.cpp

 *********************************************************************/

#ifndef __AUDACITY_REALTIMEEFFECTSTATE_H__
#define __AUDACITY_REALTIMEEFFECTSTATE_H__

#include <atomic>
#include <memory>
#include <vector>
#include <cstddef>
#include "GlobalVariable.h"
#include "ModuleInterface.h" // for PluginID

class EffectProcessor;

class RealtimeEffectState
{
public:
   struct AUDACITY_DLL_API EffectFactory : GlobalHook<EffectFactory,
      std::unique_ptr<EffectProcessor>(const PluginID &)
   >{};

   explicit RealtimeEffectState(const PluginID & id);
   ~RealtimeEffectState();

   //! May be called with nonempty id at most once in the lifetime of a state
   /*!
    Call with empty id is ignored.
    Called by the constructor that takes an id */
   void SetID(const PluginID & id);
   EffectProcessor *GetEffect();

   bool Suspend();
   bool Resume() noexcept;

   //! Main thread sets up for playback
   bool Initialize(double rate);
   bool AddTrack(int group, unsigned chans, float rate);
   //! Worker thread begins a batch of samples
   bool ProcessStart();
   //! Worker thread processes part of a batch of samples
   size_t Process(int group,
      unsigned chans, float **inbuf, float **outbuf, size_t numSamples);
   //! Worker thread finishes a batch of samples
   bool ProcessEnd();
   bool IsActive() const noexcept;
   //! Main thread cleans up playback
   bool Finalize();

private:
   PluginID mID;
   std::unique_ptr<EffectProcessor> mEffect;

   std::vector<int> mGroupProcessor;
   size_t mCurrentProcessor{ 0 };

   std::atomic<int> mSuspendCount{ 1 };    // Effects are initially suspended
};

#endif // __AUDACITY_REALTIMEEFFECTSTATE_H__

