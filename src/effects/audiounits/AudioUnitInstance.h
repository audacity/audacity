/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudioUnitInstance.h

  Dominic Mazzoni
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_AUDIO_UNIT_INSTANCE__
#define __AUDACITY_AUDIO_UNIT_INSTANCE__

#include "../PerTrackEffect.h"
#include "AudioUnitWrapper.h"
struct AudioUnitEvent;

class AudioUnitInstance : public PerTrackEffect::Instance
   , public AudioUnitWrapper
{
public:
   AudioUnitInstance(const PerTrackEffect &effect,
      AudioComponent component, Parameters &parameters,
      const wxString &identifier,
      unsigned audioIns, unsigned audioOuts, bool useLatency);

   void EventListener(const AudioUnitEvent *inEvent,
      AudioUnitParameterValue inParameterValue);

private:
   size_t InitialBlockSize() const;
   sampleCount GetLatency(const EffectSettings &settings, double sampleRate)
      override;

   size_t GetBlockSize() const override;
   size_t SetBlockSize(size_t maxBlockSize) override;

   bool ProcessInitialize(EffectSettings &settings, double sampleRate,
      sampleCount totalLen, ChannelNames chanMap) override;
   bool ProcessFinalize() override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
      override;

   bool RealtimeInitialize(EffectSettings &settings, double sampleRate)
      override;
   bool RealtimeAddProcessor(EffectSettings &settings,
      unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize(EffectSettings &settings) noexcept override;
   bool RealtimeSuspend() override;
   bool RealtimeResume() override;
   bool RealtimeProcessStart(EffectSettings &settings) override;
   size_t RealtimeProcess(size_t group, EffectSettings &settings,
      const float *const *inbuf, float *const *outbuf, size_t numSamples)
      override;
   bool RealtimeProcessEnd(EffectSettings &settings) noexcept override;

   bool SetRateAndChannels(double sampleRate);

   static OSStatus RenderCallback(void *inRefCon,
      AudioUnitRenderActionFlags *inActionFlags,
      const AudioTimeStamp *inTimeStamp,
      UInt32 inBusNumber, UInt32 inNumFrames, AudioBufferList *ioData);
   OSStatus Render(AudioUnitRenderActionFlags *inActionFlags,
      const AudioTimeStamp *inTimeStamp,
      UInt32 inBusNumber, UInt32 inNumFrames, AudioBufferList *ioData);

   bool BypassEffect(bool bypass);

private:
   //! Whether the master instance is now allocated to a group number
   bool mRecruited{ false };
   std::vector<std::unique_ptr<AudioUnitInstance>> mSlaves;

   AudioUnitCleanup<AudioUnit, AudioUnitUninitialize> mInitialization;
   AudioTimeStamp mTimeStamp{};
   PackedArray::Ptr<AudioBufferList> mInputList;
   PackedArray::Ptr<AudioBufferList> mOutputList;


   const wxString &mIdentifier; // for debug messages only
   const size_t mBlockSize;
   const unsigned mAudioIns;
   const unsigned mAudioOuts;
   const bool mUseLatency;
   bool mLatencyDone{ false };
};
#endif
