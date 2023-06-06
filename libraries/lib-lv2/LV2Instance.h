/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2Instance.h
  @brief Holds non-UI processing state for an instance of an LV2 effect

  Paul Licameli split from LV2Effect.h

  Audacity(R) is copyright (c) 1999-2013 Audacity Team.
  License: GPL v2 or later.  See License.txt.

*********************************************************************/

#ifndef __AUDACITY_LV2_INSTANCE__
#define __AUDACITY_LV2_INSTANCE__


#if USE_LV2

#include "LV2InstanceFeaturesList.h"
#include "LV2Ports.h"
#include "PerTrackEffect.h"

class LV2Wrapper;

class LV2_API LV2Instance final : public PerTrackEffect::Instance
{
public:
   LV2Instance(const PerTrackEffect &effect,
      const LV2FeaturesList &features, const LV2Ports &ports);
   ~LV2Instance() override;

   bool IsOk() const { return mFeatures.mOk; }

   const LV2PortStates &GetPortStates() const { return mPortStates; }

   bool ProcessInitialize(EffectSettings &settings, double sampleRate,
      ChannelNames chanMap) override;
   size_t ProcessBlock(EffectSettings &settings,
      const float *const *inBlock, float *const *outBlock, size_t blockLen)
   override;
   SampleCount GetLatency(
      const EffectSettings &settings, double sampleRate) const override;

   const LV2Wrapper *GetMaster() const { return mMaster.get(); }

   //! Do nothing if there is already an LV2Wrapper with the desired rate.
   //! The wrapper object remains until this is destroyed
   //! or the wrapper is re-made with another rate.
   void MakeMaster(const EffectSettings &settings, double sampleRate);

   std::unique_ptr<LV2Wrapper> MakeWrapper(const EffectSettings &settings,
      double sampleRate, EffectOutputs *pOutputs);

   size_t GetBlockSize() const override;
   size_t SetBlockSize(size_t maxBlockSize) override;

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;

   bool RealtimeInitialize(EffectSettings &settings, double sampleRate)
      override;
   bool RealtimeAddProcessor(EffectSettings& settings, EffectOutputs *pOutputs,
      unsigned numChannels, float sampleRate) override;
   bool RealtimeFinalize(EffectSettings &settings) noexcept override;
   bool RealtimeSuspend() override;
   bool RealtimeResume() override;
   bool RealtimeProcessStart(MessagePackage &package) override;
   size_t RealtimeProcess(size_t group,  EffectSettings &settings,
      const float *const *inbuf, float *const *outbuf, size_t numSamples)
      override;
   bool RealtimeProcessEnd(EffectSettings &settings) noexcept override;

private:
   LV2InstanceFeaturesList mFeatures;
   const LV2Ports &mPorts;
   LV2PortStates mPortStates{ mPorts };

   //! Holds lv2 library state for destructive processing
   std::unique_ptr<LV2Wrapper> mMaster;

   //! Each holds lv2 library state for realtime processing of one track
   std::vector<std::unique_ptr<LV2Wrapper>> mSlaves;

   LV2_Atom_Forge mForge{};

   // Position info
   float mPositionSpeed{ 1.0f };
   int64_t mPositionFrame{ 0 };

   size_t mUserBlockSize{};

   size_t mNumSamples{};
   bool mRolling{ true };
   bool mUseLatency{ false };
};

#endif
#endif
