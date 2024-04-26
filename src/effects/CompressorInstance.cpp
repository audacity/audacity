/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompressorInstance.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "CompressorInstance.h"
#include "processors/CompressorProcessor.h"

CompressorInstance::CompressorInstance(const PerTrackEffect& effect)
    : PerTrackEffect::Instance { effect }
    , mCompressor { std::make_unique<CompressorProcessor>() }
{
}

CompressorInstance::CompressorInstance(CompressorInstance&& other)
    : PerTrackEffect::Instance { other }
    , mCompressor { std::move(other.mCompressor) }
    , mSlaves { std::move(other.mSlaves) }
{
}

bool CompressorInstance::ProcessInitialize(
   EffectSettings& settings, double sampleRate, ChannelNames chanMap)
{
   InstanceInit(settings, *mCompressor, GetAudioInCount(), sampleRate);
   return true;
}

size_t CompressorInstance::ProcessBlock(
   EffectSettings& settings, const float* const* inBlock,
   float* const* outBlock, size_t blockLen)
{
   return InstanceProcess(settings, *mCompressor, inBlock, outBlock, blockLen);
}

bool CompressorInstance::RealtimeInitialize(EffectSettings&, double)
{
   SetBlockSize(512);
   mSlaves.clear();
   return true;
}

bool CompressorInstance::RealtimeSuspend()
{
   return true;
}

bool CompressorInstance::RealtimeResume()
{
   for (auto& slave : mSlaves)
      // Neither block size nore sample rate or any other parameter has changed,
      // so `Reinit()` should not reallocate memory.
      slave.mCompressor->Reinit();
   return true;
}

bool CompressorInstance::RealtimeAddProcessor(
   EffectSettings& settings, EffectOutputs*, unsigned numChannels,
   float sampleRate)
{
   mSlaves.emplace_back(mProcessor);
   InstanceInit(settings, *mSlaves.back().mCompressor, numChannels, sampleRate);
   return true;
}

bool CompressorInstance::RealtimeFinalize(EffectSettings&) noexcept
{
   mSlaves.clear();
   return true;
}

size_t CompressorInstance::RealtimeProcess(
   size_t group, EffectSettings& settings, const float* const* inbuf,
   float* const* outbuf, size_t numSamples)
{
   if (group >= mSlaves.size())
      return 0;
   return InstanceProcess(
      settings, *mSlaves[group].mCompressor, inbuf, outbuf, numSamples);
}

namespace
{
void CheckSettings(
   const EffectSettings& settings, CompressorProcessor& instance)
{
   if (
      const CompressorSettings* pSettings = settings.cast<CompressorSettings>())
      instance.ApplySettingsIfNeeded(*pSettings);
   else
      instance.ApplySettingsIfNeeded(*settings.cast<LimiterSettings>());
}
} // namespace

void CompressorInstance::InstanceInit(
   EffectSettings& settings, CompressorProcessor& instance, int numChannels,
   float sampleRate)
{
   CheckSettings(settings, instance);
   instance.Init(sampleRate, numChannels, GetBlockSize());
}

size_t CompressorInstance::InstanceProcess(
   EffectSettings& settings, CompressorProcessor& instance,
   const float* const* inBlock, float* const* outBlock, size_t blockLen)
{
   CheckSettings(settings, instance);
   instance.Process(inBlock, outBlock, blockLen);
   return blockLen;
}

EffectInstance::SampleCount CompressorInstance::GetLatency(
   const EffectSettings& settings, double sampleRate) const
{
   const LimiterSettings* pSettings = settings.cast<LimiterSettings>();
   if (!pSettings)
      pSettings = settings.cast<CompressorSettings>();
   return pSettings->lookaheadMs * sampleRate / 1000;
}

unsigned CompressorInstance::GetAudioOutCount() const
{
   return 2;
}

unsigned CompressorInstance::GetAudioInCount() const
{
   return 2;
}
