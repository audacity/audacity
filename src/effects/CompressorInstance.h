/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompressorInstance.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "EffectInterface.h"
#include "PerTrackEffect.h"
#include <memory>
#include <vector>

class CompressorProcessor;

class CompressorInstance final :
    public PerTrackEffect::Instance,
    public EffectInstanceWithBlockSize
{
public:
   explicit CompressorInstance(const PerTrackEffect& effect);
   explicit CompressorInstance(CompressorInstance&& other);

private:
   bool ProcessInitialize(
      EffectSettings& settings, double sampleRate,
      ChannelNames chanMap) override;

   size_t ProcessBlock(
      EffectSettings& settings, const float* const* inBlock,
      float* const* outBlock, size_t blockLen) override;

   bool RealtimeInitialize(EffectSettings& settings, double) override;

   bool RealtimeSuspend() override;

   bool RealtimeResume() override;

   bool RealtimeAddProcessor(
      EffectSettings& settings, EffectOutputs* pOutputs, unsigned numChannels,
      float sampleRate) override;

   bool RealtimeFinalize(EffectSettings& settings) noexcept override;

   size_t RealtimeProcess(
      size_t group, EffectSettings& settings, const float* const* inbuf,
      float* const* outbuf, size_t numSamples) override;

   void InstanceInit(
      EffectSettings& settings, CompressorProcessor& instance, int numChannels,
      float sampleRate);

   size_t InstanceProcess(
      EffectSettings& settings, CompressorProcessor& instance,
      const float* const* inBlock, float* const* outBlock, size_t blockLen);

   // TODO study other virtual functions and see if there's something important.
   EffectInstance::SampleCount
   GetLatency(const EffectSettings& settings, double sampleRate) const override;

   unsigned GetAudioInCount() const override;
   unsigned GetAudioOutCount() const override;

   std::unique_ptr<CompressorProcessor> mCompressor;
   std::vector<CompressorInstance> mSlaves;
};
