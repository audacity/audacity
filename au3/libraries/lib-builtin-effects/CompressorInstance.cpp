/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompressorInstance.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "CompressorInstance.h"
#include "libraries/lib-dynamic-range-processor/CompressorProcessor.h"
#include "MathApprox.h"
#include <numeric>

CompressorInstance::CompressorInstance(const PerTrackEffect& effect)
    : PerTrackEffect::Instance{effect}
    , mCompressor{std::make_unique<CompressorProcessor>()}
{
}

CompressorInstance::CompressorInstance(CompressorInstance&& other)
    : PerTrackEffect::Instance{other}
    , mCompressor{std::move(other.mCompressor)}
    , mSlaves{std::move(other.mSlaves)}
    , mSampleCounter{std::move(other.mSampleCounter)}
    , mSampleRate{std::move(other.mSampleRate)}
    , mAudioThreadBufferSize{std::move(other.mAudioThreadBufferSize)}
    , mOutputQueue{std::move(other.mOutputQueue)}
    , mCompressionGainDbQueue{std::move(other.mCompressionGainDbQueue)}
    , mOutputDbQueue{std::move(other.mOutputDbQueue)}
{
}

const std::optional<double>& CompressorInstance::GetSampleRate() const
{
    return mSampleRate;
}

const std::optional<size_t>& CompressorInstance::GetAudioThreadBufferSize() const
{
    return mAudioThreadBufferSize;
}

float CompressorInstance::GetLatencyMs() const
{
    return mSlaves.empty()
           ? mCompressor->GetSettings().lookaheadMs
           : mSlaves.front().mCompressor->GetSettings().lookaheadMs;
}

void CompressorInstance::SetOutputQueue(
    std::weak_ptr<DynamicRangeProcessorOutputPacketQueue> outputQueue)
{
    mOutputQueue = outputQueue;
    for (auto& slave : mSlaves) {
        slave.mOutputQueue = outputQueue;
    }
}

void CompressorInstance::SetCompressionGainDbQueue(std::weak_ptr<LockFreeQueue<float> > queue)
{
    mCompressionGainDbQueue = queue;
    for (auto& slave : mSlaves) {
        slave.mCompressionGainDbQueue = queue;
    }
}

void CompressorInstance::SetOutputDbQueue(std::weak_ptr<LockFreeQueue<float> > queue)
{
    mOutputDbQueue = queue;
    for (auto& slave : mSlaves) {
        slave.mOutputDbQueue = queue;
    }
}

bool CompressorInstance::ProcessInitialize(
    EffectSettings& settings, double sampleRate, ChannelNames chanMap)
{
    mSampleRate = sampleRate;
    InstanceInit(settings, *this, GetAudioInCount(), sampleRate);
    return true;
}

bool CompressorInstance::ProcessFinalize() noexcept
{
    mSampleRate.reset();
    return true;
}

namespace {
DynamicRangeProcessorSettings
GetDynamicRangeProcessorSettings(const EffectSettings& settings)
{
    if (auto pSettings = settings.cast<CompressorSettings>()) {
        return *pSettings;
    }
    return *settings.cast<LimiterSettings>();
}

auto GetMaxDbIncrease(const float* in, const float* out, size_t blockLen)
{
    auto leastRatio = 0.f;
    for (size_t i = 0; i < blockLen; ++i) {
        const auto absIn = std::abs(in[i]);
        if (absIn < 1e-6) {
            continue;
        }
        leastRatio = std::max(std::abs(out[i]) / absIn, leastRatio);
    }
    return leastRatio == 0 ? -std::numeric_limits<float>::infinity()
           : log2ToDb* FastLog2(leastRatio);
}
} // namespace

size_t CompressorInstance::ProcessBlock(
    EffectSettings& settings, const float* const* inBlock,
    float* const* outBlock, size_t blockLen)
{
    return InstanceProcess(settings, *mCompressor, inBlock, outBlock, blockLen);
}

bool CompressorInstance::RealtimeInitialize(EffectSettings&, double sampleRate, size_t audioThreadBufferSize)
{
    SetBlockSize(512);
    mSlaves.clear();
    mSampleCounter = 0;
    mSampleRate = sampleRate;
    mAudioThreadBufferSize = audioThreadBufferSize;
    InitializeProcessingSettingsPublisher::Publish(
        std::make_optional(InitializeProcessingSettings { sampleRate, audioThreadBufferSize }));
    return true;
}

bool CompressorInstance::RealtimeResume()
{
    for (auto& slave : mSlaves) {
        // Neither block size nore sample rate or any other parameter has changed,
        // so `Reinit()` should not reallocate memory.
        slave.mCompressor->Reinit();
    }
    RealtimeResumePublisher::Publish({});
    return true;
}

bool CompressorInstance::RealtimeAddProcessor(
    EffectSettings& settings, EffectOutputs* pOutputs, unsigned numChannels,
    float sampleRate)
{
    mSlaves.emplace_back(mProcessor);
    InstanceInit(settings, mSlaves.back(), numChannels, sampleRate);
    return true;
}

bool CompressorInstance::RealtimeFinalize(EffectSettings&) noexcept
{
    mSlaves.clear();
    mSampleRate.reset();
    InitializeProcessingSettingsPublisher::Publish(std::nullopt);
    return true;
}

namespace {
float GetOutputDb(
    const CompressorProcessor::FrameStats& stats,
    const DynamicRangeProcessorSettings& settings)
{
    return stats.maxInputSampleDb + stats.dbGainOfMaxInputSample
           + CompressorProcessor::GetMakeupGainDb(settings);
}
} // namespace

size_t CompressorInstance::RealtimeProcess(
    size_t group, EffectSettings& settings, const float* const* inbuf,
    float* const* outbuf, size_t numSamples)
{
    if (group >= mSlaves.size()) {
        return 0;
    }
    auto& slave = mSlaves[group];
    auto& compressor = *slave.mCompressor;
    const auto numProcessedSamples
        =InstanceProcess(settings, compressor, inbuf, outbuf, numSamples);
    if (const auto queue = slave.mOutputQueue.lock()) {
        const auto& frameStats = compressor.GetLastFrameStats();
        const auto& compressorSettings = compressor.GetSettings();
        const float netGain = compressorSettings.outCompressionThreshDb
                              - compressorSettings.inCompressionThreshDb;
        const auto targetCompressionDb
            =compressor.EvaluateTransferFunction(frameStats.maxInputSampleDb)
              - frameStats.maxInputSampleDb - netGain;
        DynamicRangeProcessorOutputPacket newPacket;
        newPacket.indexOfFirstSample = slave.mSampleCounter;
        newPacket.numSamples = numProcessedSamples;
        newPacket.targetCompressionDb = targetCompressionDb;
        newPacket.actualCompressionDb = frameStats.dbGainOfMaxInputSample;
        newPacket.inputDb = frameStats.maxInputSampleDb;
        newPacket.outputDb = GetOutputDb(frameStats, compressorSettings);
        queue->Put(newPacket);
    }

    if (const auto queue = slave.mCompressionGainDbQueue.lock()) {
        queue->Put(compressor.GetLastFrameStats().dbGainOfMaxInputSample);
    }

    if (const auto queue = slave.mOutputDbQueue.lock()) {
        queue->Put(GetOutputDb(compressor.GetLastFrameStats(), compressor.GetSettings()));
    }

    slave.mSampleCounter += numProcessedSamples;
    return numProcessedSamples;
}

void CompressorInstance::RealtimePassThrough(
    size_t group, EffectSettings& settings, const float* const* inbuf,
    size_t numSamples)
{
    // Keep track of the amount of samples that passed by, so that when
    // processing resumes, the visualization reflects the elapsed time while
    // bypassed.
    if (group < mSlaves.size()) {
        mSlaves[group].mSampleCounter += numSamples;
    }
}

void CompressorInstance::InstanceInit(
    EffectSettings& settings, CompressorInstance& instance, int numChannels,
    float sampleRate)
{
    instance.mOutputQueue = mOutputQueue;
    instance.mCompressionGainDbQueue = mCompressionGainDbQueue;
    instance.mOutputDbQueue = mOutputDbQueue;
    instance.mCompressor->ApplySettingsIfNeeded(
        GetDynamicRangeProcessorSettings(settings));
    instance.mCompressor->Init(sampleRate, numChannels, GetBlockSize());
}

size_t CompressorInstance::InstanceProcess(
    EffectSettings& settings, CompressorProcessor& instance,
    const float* const* inBlock, float* const* outBlock, size_t blockLen)
{
    instance.ApplySettingsIfNeeded(GetDynamicRangeProcessorSettings(settings));
    instance.Process(inBlock, outBlock, blockLen);
    return blockLen;
}

EffectInstance::SampleCount CompressorInstance::GetLatency(
    const EffectSettings& settings, double sampleRate) const
{
    return GetDynamicRangeProcessorSettings(settings).lookaheadMs * sampleRate
           / 1000;
}

unsigned CompressorInstance::GetAudioOutCount() const
{
    return 2;
}

unsigned CompressorInstance::GetAudioInCount() const
{
    return 2;
}
