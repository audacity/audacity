/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompressorInstance.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "CompressorProcessor.h"
#include "DynamicRangeProcessorTypes.h"
#include "EffectInterface.h"
#include "Observer.h"
#include "PerTrackEffect.h"
#include <memory>
#include <optional>
#include <vector>

using InitializeProcessingSettingsPublisher
    =Observer::Publisher<std::optional<InitializeProcessingSettings> >;
using RealtimeResumePublisher = Observer::Publisher<Unbypassed>;

class BUILTIN_EFFECTS_API CompressorInstance final : public PerTrackEffect::Instance, public EffectInstanceWithBlockSize,
    public InitializeProcessingSettingsPublisher, public RealtimeResumePublisher
{
public:
    explicit CompressorInstance(const PerTrackEffect& effect);
    explicit CompressorInstance(CompressorInstance&& other);

    const std::optional<double>& GetSampleRate() const;
    float GetLatencyMs() const;
    void SetOutputQueue(std::weak_ptr<DynamicRangeProcessorOutputPacketQueue>);
    void SetMeterValuesQueue(
        std::weak_ptr<DynamicRangeProcessorMeterValuesQueue> queue);

private:
    bool ProcessInitialize(
        EffectSettings& settings, double sampleRate, ChannelNames chanMap) override;

    bool ProcessFinalize() noexcept override;

    size_t ProcessBlock(
        EffectSettings& settings, const float* const* inBlock, float* const* outBlock, size_t blockLen) override;

    bool
    RealtimeInitialize(EffectSettings& settings, double sampleRate) override;

    bool RealtimeResume() override;

    bool RealtimeAddProcessor(
        EffectSettings& settings, EffectOutputs* pOutputs, unsigned numChannels, float sampleRate) override;

    bool RealtimeFinalize(EffectSettings& settings) noexcept override;

    size_t RealtimeProcess(
        size_t group, EffectSettings& settings, const float* const* inbuf, float* const* outbuf, size_t numSamples) override;

    void RealtimePassThrough(
        size_t group, EffectSettings& settings, const float* const* inbuf, size_t numSamples) override;

    void InstanceInit(
        EffectSettings& settings, CompressorInstance& instance, int numChannels, float sampleRate);

    size_t InstanceProcess(
        EffectSettings& settings, CompressorProcessor& instance, const float* const* inBlock, float* const* outBlock, size_t blockLen);

    EffectInstance::SampleCount
    GetLatency(const EffectSettings& settings, double sampleRate) const override;

    unsigned GetAudioInCount() const override;
    unsigned GetAudioOutCount() const override;

    std::unique_ptr<CompressorProcessor> mCompressor;
    std::vector<CompressorInstance> mSlaves;
    long long mSampleCounter = 0;
    std::optional<double> mSampleRate;
    std::weak_ptr<DynamicRangeProcessorOutputPacketQueue> mOutputQueue;
    std::weak_ptr<DynamicRangeProcessorMeterValuesQueue> mCompressionValueQueue;
};
