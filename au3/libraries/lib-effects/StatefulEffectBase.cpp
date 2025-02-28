/**********************************************************************

  Audacity: A Digital Audio Editor

  StatefulEffectBase.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

  Paul Licameli split from Effect.cpp

**********************************************************************/
#include "StatefulEffectBase.h"
#include "SampleCount.h"

StatefulEffectBase::Instance::Instance(StatefulEffectBase& effect)
    : mEffect{effect}
{
}

StatefulEffectBase::Instance::~Instance() = default;

bool StatefulEffectBase::Instance::Init()
{
    return GetEffect().Init();
}

bool StatefulEffectBase::Instance::RealtimeInitialize(
    EffectSettings& settings, double sampleRate)
{
    return GetEffect().RealtimeInitialize(settings, sampleRate);
}

bool StatefulEffectBase::Instance::
RealtimeAddProcessor(EffectSettings& settings,
                     EffectOutputs* pOutputs, unsigned numChannels, float sampleRate)
{
    return GetEffect()
           .RealtimeAddProcessor(settings, pOutputs, numChannels, sampleRate);
}

bool StatefulEffectBase::Instance::RealtimeSuspend()
{
    return GetEffect().RealtimeSuspend();
}

bool StatefulEffectBase::Instance::RealtimeResume()
{
    return GetEffect().RealtimeResume();
}

bool StatefulEffectBase::Instance::RealtimeProcessStart(
    MessagePackage& package)
{
    return GetEffect().RealtimeProcessStart(package);
}

size_t StatefulEffectBase::Instance::RealtimeProcess(size_t group,
                                                     EffectSettings& settings,
                                                     const float* const* inBuf, float* const* outBuf, size_t numSamples)
{
    return GetEffect()
           .RealtimeProcess(group, settings, inBuf, outBuf, numSamples);
}

bool StatefulEffectBase::Instance::RealtimeProcessEnd(EffectSettings& settings) noexcept
{
    return GetEffect().RealtimeProcessEnd(settings);
}

bool StatefulEffectBase::Instance::RealtimeFinalize(EffectSettings& settings) noexcept
{
    return GetEffect().RealtimeFinalize(settings);
}

size_t StatefulEffectBase::Instance::GetBlockSize() const
{
    return GetEffect().GetBlockSize();
}

size_t StatefulEffectBase::Instance::SetBlockSize(size_t maxBlockSize)
{
    return GetEffect().SetBlockSize(maxBlockSize);
}

unsigned StatefulEffectBase::Instance::GetAudioInCount() const
{
    return GetEffect().GetAudioInCount();
}

unsigned StatefulEffectBase::Instance::GetAudioOutCount() const
{
    return GetEffect().GetAudioOutCount();
}

bool StatefulEffectBase::Instance::NeedsDither() const
{
    return GetEffect().NeedsDither();
}

bool StatefulEffectBase::Instance::ProcessInitialize(
    EffectSettings& settings, double sampleRate, ChannelNames chanMap)
{
    return GetEffect()
           .ProcessInitialize(settings, sampleRate, chanMap);
}

bool StatefulEffectBase::Instance::ProcessFinalize() noexcept
{
    return GetEffect().ProcessFinalize();
}

size_t StatefulEffectBase::SetBlockSize(size_t maxBlockSize)
{
    mEffectBlockSize = maxBlockSize;
    return mEffectBlockSize;
}

size_t StatefulEffectBase::GetBlockSize() const
{
    return mEffectBlockSize;
}

unsigned StatefulEffectBase::GetAudioInCount() const
{
    return 0;
}

unsigned StatefulEffectBase::GetAudioOutCount() const
{
    return 0;
}

bool StatefulEffectBase::RealtimeInitialize(EffectSettings&, double)
{
    return false;
}

bool StatefulEffectBase::RealtimeAddProcessor(EffectSettings& settings,
                                              EffectOutputs*, unsigned numChannels, float sampleRate)
{
    return true;
}

bool StatefulEffectBase::RealtimeSuspend()
{
    return true;
}

bool StatefulEffectBase::RealtimeResume()
{
    return true;
}

bool StatefulEffectBase::RealtimeProcessStart(MessagePackage&)
{
    return true;
}

size_t StatefulEffectBase::RealtimeProcess(size_t group,
                                           EffectSettings& settings,
                                           const float* const* inbuf, float* const* outbuf, size_t numSamples)
{
    return 0;
}

bool StatefulEffectBase::RealtimeProcessEnd(EffectSettings& settings) noexcept
{
    return true;
}

bool StatefulEffectBase::RealtimeFinalize(EffectSettings& settings) noexcept
{
    return false;
}

bool StatefulEffectBase::Init()
{
    return true;
}

sampleCount StatefulEffectBase::GetLatency() const
{
    return 0;
}

bool StatefulEffectBase::NeedsDither() const
{
    return true;
}

bool StatefulEffectBase::ProcessInitialize(
    EffectSettings&, double, ChannelNames)
{
    return true;
}

bool StatefulEffectBase::ProcessFinalize() noexcept
{
    return true;
}
