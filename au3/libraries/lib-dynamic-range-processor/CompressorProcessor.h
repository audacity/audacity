/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompressorProcessor.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "DynamicRangeProcessorTypes.h"
#include <array>
#include <limits>
#include <memory>
#include <vector>

namespace DanielRudrich {
class GainReductionComputer;
class LookAheadGainReduction;
} // namespace DanielRudrich

class DYNAMIC_RANGE_PROCESSOR_API CompressorProcessor
{
public:
    struct FrameStats
    {
        float maxInputSampleDb = -std::numeric_limits<float>::infinity();
        float dbGainOfMaxInputSample = 0;
    };

    static float
    GetMaxCompressionDb(const DynamicRangeProcessorSettings& settings);
    static float GetMakeupGainDb(const DynamicRangeProcessorSettings& settings);
    static float EvaluateTransferFunction(
        const DynamicRangeProcessorSettings& settings, float inputDb);

    CompressorProcessor(
        const DynamicRangeProcessorSettings& settings = {
        // No reason to initialize with compressor settings other than
        // convenience.
        CompressorSettings {}
    });
    CompressorProcessor(const CompressorProcessor& other) = delete;
    ~CompressorProcessor();

    void ApplySettingsIfNeeded(const DynamicRangeProcessorSettings& settings);
    void Init(int sampleRate, int numChannels, int blockSize);
    void Reinit();
    const DynamicRangeProcessorSettings& GetSettings() const;
    void
    Process(const float* const* inBlock, float* const* outBlock, int blockLen);
    const std::vector<std::vector<float> >& GetDelayedInput() const;
    const FrameStats& GetLastFrameStats() const;
    float EvaluateTransferFunction(float inputDb) const;

private:
    void UpdateEnvelope(const float* const* inBlock, int blockLen);
    void CopyWithDelay(const float* const* inBlock, int blockLen);
    void ApplyEnvelope(
        float* const* outBlock, int blockLen, float& delayedInputMax, int& delayedInputMaxIndex);
    bool Initialized() const;

    static constexpr auto maxBlockSize = 512;

    const std::unique_ptr<DanielRudrich::GainReductionComputer>
    mGainReductionComputer;
    const std::unique_ptr<DanielRudrich::LookAheadGainReduction>
    mLookAheadGainReduction;
    DynamicRangeProcessorSettings mSettings;
    int mSampleRate = 0;
    int mNumChannels = 0;
    int mBlockSize = 0;
    std::array<float, maxBlockSize> mEnvelope;
    std::vector<std::vector<float> >
    mDelayedInput;   // Can't conveniently use an array here, because neither
                     // delay time nor sample rate are known at compile time.
                     // Re-allocation during playback is only done if the user
                     // changes the look-ahead settings, in which case glitches
                     // are hardly avoidable anyway.
    FrameStats mLastFrameStats;
};
