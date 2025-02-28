/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompressorProcessor.cpp

  Matthieu Hodgkinson

**********************************************************************/

#include "CompressorProcessor.h"
#include "MathApprox.h"
#include "SimpleCompressor/GainReductionComputer.h"
#include "SimpleCompressor/LookAheadGainReduction.h"
#include <algorithm>
#include <cassert>

float CompressorProcessor::GetMakeupGainDb(
    const DynamicRangeProcessorSettings& settings)
{
    return settings.outCompressionThreshDb - settings.inCompressionThreshDb;
}

float CompressorProcessor::GetMaxCompressionDb(
    const DynamicRangeProcessorSettings& settings)
{
    const auto tfEval = EvaluateTransferFunction(settings, 0);
    const auto netGain
        =settings.outCompressionThreshDb - settings.inCompressionThreshDb;
    return netGain - tfEval;
}

float CompressorProcessor::EvaluateTransferFunction(
    const DynamicRangeProcessorSettings& settings, float inputDb)
{
    return DanielRudrich::GainReductionComputer::getCharacteristicSample(
        inputDb, settings.kneeWidthDb, settings.inCompressionThreshDb,
        settings.compressionRatio, GetMakeupGainDb(settings));
}

CompressorProcessor::CompressorProcessor(
    const DynamicRangeProcessorSettings& settings)
    : mGainReductionComputer{std::make_unique<
                                 DanielRudrich::GainReductionComputer>()}
    , mLookAheadGainReduction{std::make_unique<
                                  DanielRudrich::LookAheadGainReduction>()}
    , mSettings{settings}
{
}

CompressorProcessor::~CompressorProcessor() = default;

void CompressorProcessor::ApplySettingsIfNeeded(
    const DynamicRangeProcessorSettings& settings)
{
    if (settings == mSettings) {
        return;
    }

    const auto lookaheadNeedsReinit
        =settings.lookaheadMs != mSettings.lookaheadMs;
    mSettings = settings;

    mGainReductionComputer->setThreshold(settings.inCompressionThreshDb);
    mGainReductionComputer->setKnee(settings.kneeWidthDb);
    mGainReductionComputer->setAttackTime(settings.attackMs / 1000);
    mGainReductionComputer->setReleaseTime(settings.releaseMs / 1000);
    mGainReductionComputer->setRatio(settings.compressionRatio);
    mGainReductionComputer->setMakeUpGain(GetMakeupGainDb(settings));

    if (lookaheadNeedsReinit) {
        mLookAheadGainReduction->setDelayTime(settings.lookaheadMs / 1000);
        Reinit();
    }
}

void CompressorProcessor::Init(int sampleRate, int numChannels, int blockSize)
{
    mSampleRate = sampleRate;
    mNumChannels = numChannels;
    mBlockSize = std::min(blockSize, maxBlockSize);
    Reinit();
}

const DynamicRangeProcessorSettings& CompressorProcessor::GetSettings() const
{
    return mSettings;
}

void CompressorProcessor::Process(
    const float* const* inBlock, float* const* outBlock, int blockLen)
{
    assert(Initialized());
    if (!Initialized()) {
        return;
    }

    auto processed = 0;
    mLastFrameStats = {};
    std::vector<const float*> in(mNumChannels);
    std::vector<float*> out(mNumChannels);
    while (processed < blockLen)
    {
        for (auto i = 0; i < mNumChannels; ++i) {
            in[i] = inBlock[i] + processed;
            out[i] = outBlock[i] + processed;
        }
        const auto toProcess = std::min(blockLen - processed, mBlockSize);
        UpdateEnvelope(in.data(), toProcess);
        CopyWithDelay(in.data(), toProcess);

        float delayedInputAbsMax = 0;
        int delayedInputAbsMaxIndex = 0;
        ApplyEnvelope(
            out.data(), toProcess, delayedInputAbsMax, delayedInputAbsMaxIndex);

        const auto blockMaxDb = log2ToDb * FastLog2(delayedInputAbsMax);
        if (mLastFrameStats.maxInputSampleDb < blockMaxDb) {
            mLastFrameStats.maxInputSampleDb = blockMaxDb;
            mLastFrameStats.dbGainOfMaxInputSample
                =mEnvelope[delayedInputAbsMaxIndex];
        }

        processed += toProcess;
    }
}

const std::vector<std::vector<float> >&
CompressorProcessor::GetDelayedInput() const
{
    return mDelayedInput;
}

const CompressorProcessor::FrameStats&
CompressorProcessor::GetLastFrameStats() const
{
    return mLastFrameStats;
}

float CompressorProcessor::EvaluateTransferFunction(float inputDb) const
{
    return mGainReductionComputer->getCharacteristicSample(inputDb);
}

void CompressorProcessor::UpdateEnvelope(const float* const* in, int blockLen)
{
    // Fill mEnvelope with max of all in channels;
    for (auto i = 0; i < blockLen; ++i) {
        auto max = 0.f;
        for (auto j = 0; j < mNumChannels; ++j) {
            const auto x = std::abs(in[j][i]);
            if (x > max) {
                max = x;
            }
        }
        mEnvelope[i] = max;
    }

    // TODO: uses std::log10 ; use log2 optimization instead.
    mGainReductionComputer->computeGainInDecibelsFromSidechainSignal(
        mEnvelope.data(), mEnvelope.data(), blockLen);

    if (mSettings.lookaheadMs <= 0) {
        return;
    }

    mLookAheadGainReduction->pushSamples(mEnvelope.data(), blockLen);
    mLookAheadGainReduction->process();
    mLookAheadGainReduction->readSamples(mEnvelope.data(), blockLen);
}

void CompressorProcessor::CopyWithDelay(const float* const* in, int blockLen)
{
    const auto d = mLookAheadGainReduction->getDelayInSamples();
    for (auto i = 0; i < mNumChannels; ++i) {
        std::copy(in[i], in[i] + blockLen, mDelayedInput[i].data() + d);
    }
}

void CompressorProcessor::ApplyEnvelope(
    float* const* out, int blockLen, float& delayedInputAbsMax,
    int& delayedInputAbsMaxIndex)
{
    const auto makeupGainDb = mGainReductionComputer->getMakeUpGain();
    const auto d = mLookAheadGainReduction->getDelayInSamples();
    std::array<float, 2> chanAbsMax { 0.f, 0.f };
    std::array<int, 2> chanAbsMaxIndex { 0, 0 };
    for (auto i = 0; i < mNumChannels; ++i) {
        const auto in = mDelayedInput[i].data();
        for (auto j = 0; j < blockLen; ++j) {
            if (std::abs(in[j]) > chanAbsMax[i]) {
                chanAbsMax[i] = std::abs(in[j]);
                chanAbsMaxIndex[i] = j;
            }
            out[i][j]
                =in[j] * std::pow(10.f, 0.05f * (mEnvelope[j] + makeupGainDb));
        }
        std::move(in + blockLen, in + blockLen + d, in);
    }
    const auto i = chanAbsMax[0] > chanAbsMax[1] ? 0 : 1;
    delayedInputAbsMax = chanAbsMax[i];
    delayedInputAbsMaxIndex = chanAbsMaxIndex[i];
}

void CompressorProcessor::Reinit()
{
    if (!Initialized()) {
        // Not there yet.
        return;
    }
    mGainReductionComputer->prepare(mSampleRate);
    // In this order: setDelayTime, then prepare:
    mLookAheadGainReduction->setDelayTime(mSettings.lookaheadMs / 1000);
    mLookAheadGainReduction->prepare(mSampleRate, mBlockSize);
    const auto maxDelay
        =std::max(compressorMaxLookaheadMs, limiterMaxLookaheadMs) * mSampleRate
          / 1000;
    const auto d = mLookAheadGainReduction->getDelayInSamples();
    assert(d <= maxDelay);
    mDelayedInput.resize(mNumChannels);
    std::for_each(mDelayedInput.begin(), mDelayedInput.end(), [&](auto& v) {
        v.reserve(maxDelay + mBlockSize);
        v.resize(d + mBlockSize);
        std::fill(v.begin(), v.end(), 0.f);
    });
    std::fill(mEnvelope.begin(), mEnvelope.end(), 0.f);
}

bool CompressorProcessor::Initialized() const
{
    return mSampleRate != 0 && mNumChannels != 0 && mBlockSize != 0;
}
