/**********************************************************************

  Audacity: A Digital Audio Editor

  StatefulPerTrackEffect.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

  Paul Licameli split from PerTrackEffect.cpp

*******************************************************************//**

\class StatefulPerTrackEffect
\brief Base class for many of the effects in Audacity.

*//*******************************************************************/


#include "StatefulPerTrackEffect.h"

StatefulPerTrackEffect::Instance::~Instance() = default;

bool StatefulPerTrackEffect::Instance::ProcessInitialize(
   EffectSettings &settings, double sampleRate,
   sampleCount totalLen, ChannelNames chanMap)
{
   return GetEffect()
      .ProcessInitialize(settings, sampleRate, totalLen, chanMap);
}

bool StatefulPerTrackEffect::Instance::ProcessFinalize() /* noexcept */
{
   return GetEffect().ProcessFinalize();
}

size_t StatefulPerTrackEffect::Instance::ProcessBlock(EffectSettings &settings,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   return GetEffect().ProcessBlock(settings, inBlock, outBlock, blockLen);
}

sampleCount StatefulPerTrackEffect::Instance::GetLatency(
   const EffectSettings &, double)
{
   return GetEffect().GetLatency();
}

std::shared_ptr<EffectInstance> StatefulPerTrackEffect::MakeInstance() const
{
   // Cheat with const-cast to return an object that calls through to
   // non-const methods of a stateful effect.
   // Stateless effects should override this function and be really const
   // correct.
   return std::make_shared<Instance>(
      const_cast<StatefulPerTrackEffect&>(*this));
}

bool StatefulPerTrackEffect::Process(
   EffectInstance &instance, EffectSettings &settings)
{
   // Call through to a non-virtual function
   return PerTrackEffect::Process(instance, settings);
}

size_t StatefulPerTrackEffect::SetBlockSize(size_t maxBlockSize)
{
   mBlockSize = maxBlockSize;
   return mBlockSize;
}

size_t StatefulPerTrackEffect::GetBlockSize() const
{
   return mBlockSize;
}

sampleCount StatefulPerTrackEffect::GetLatency()
{
   return 0;
}

bool StatefulPerTrackEffect::ProcessInitialize(
   EffectSettings &, double, sampleCount, ChannelNames)
{
   return true;
}

bool StatefulPerTrackEffect::ProcessFinalize()
{
   return true;
}

size_t StatefulPerTrackEffect::ProcessBlock(EffectSettings &settings,
   const float *const *inBlock, float *const *outBlock, size_t blockLen)
{
   return 0;
}
