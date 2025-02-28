/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorTypes.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "LockFreeQueue.h"
#include <array>
#include <limits>
#include <memory>
#include <vector>

struct DynamicRangeProcessorOutputPacket
{
    long long indexOfFirstSample = 0;
    int numSamples = 0;
    float targetCompressionDb = 0;
    float actualCompressionDb = 0;
    float inputDb = 0;
    float outputDb = 0;
};

using DynamicRangeProcessorOutputPacketQueue
    =LockFreeQueue<DynamicRangeProcessorOutputPacket>;

struct MeterValues
{
    float compressionGainDb = 0;
    float outputDb = std::numeric_limits<float>::lowest();
};

using DynamicRangeProcessorMeterValuesQueue = LockFreeQueue<MeterValues>;

struct InitializeProcessingSettings
{
    explicit InitializeProcessingSettings(double sampleRate)
        : sampleRate{sampleRate}
    {
    }

    double sampleRate;
};

struct Unbypassed
{
};

static constexpr auto compressorMeterUpdatePeriodMs = 1000 / 30;

constexpr double compressorThresholdDbDefault = -10;
constexpr double compressorMakeupGainDbDefault = 0;
constexpr double compressorKneeWidthDbDefault = 5;
constexpr double compressorCompressionRatioDefault = 10;
constexpr double compressorLookaheadMsDefault = 1;
constexpr double compressorAttackMsDefault = 30;
constexpr double compressorReleaseMsDefault = 150;
constexpr double compressorMaxLookaheadMs = 1000.;

constexpr double limiterThresholdDbDefault = -5;
constexpr double limiterMakeupTargetDbDefault = -1;
constexpr double limiterKneeWidthDbDefault = 2;
constexpr double limiterLookaheadMsDefault = 1;
constexpr double limiterReleaseMsDefault = 20;
constexpr double limiterMaxLookaheadMs = 50;

constexpr double showInputDefault = 0;
constexpr double showOutputDefault = 1;
constexpr double showActualDefault = 1;
constexpr double showTargetDefault = 0;

struct CompressorSettings
{
    double thresholdDb = compressorThresholdDbDefault;
    double makeupGainDb = compressorMakeupGainDbDefault;
    double kneeWidthDb = compressorKneeWidthDbDefault;
    double compressionRatio = compressorCompressionRatioDefault;
    double lookaheadMs = compressorLookaheadMsDefault;
    double attackMs = compressorAttackMsDefault;
    double releaseMs = compressorReleaseMsDefault;
    double showInput = showInputDefault;
    double showOutput = showOutputDefault;
    double showActual = showActualDefault;
    double showTarget = showTargetDefault;
};

struct LimiterSettings
{
    double thresholdDb = limiterThresholdDbDefault;
    double makeupTargetDb = limiterMakeupTargetDbDefault;
    double kneeWidthDb = limiterKneeWidthDbDefault;
    double lookaheadMs = limiterLookaheadMsDefault;
    double releaseMs = limiterReleaseMsDefault;
    double showInput = showInputDefault;
    double showOutput = showOutputDefault;
    double showActual = showActualDefault;
    double showTarget = showTargetDefault;
};

struct DynamicRangeProcessorSettings
{
    DynamicRangeProcessorSettings(const CompressorSettings& compressorSettings)
        : inCompressionThreshDb{compressorSettings.thresholdDb}
        , outCompressionThreshDb{compressorSettings.thresholdDb
                                 + compressorSettings.makeupGainDb}
        , kneeWidthDb{compressorSettings.kneeWidthDb}
        , compressionRatio{compressorSettings.compressionRatio}
        , lookaheadMs{compressorSettings.lookaheadMs}
        , attackMs{compressorSettings.attackMs}
        , releaseMs{compressorSettings.releaseMs}
        , showInput{compressorSettings.showInput}
        , showOutput{compressorSettings.showOutput}
        , showActual{compressorSettings.showActual}
        , showTarget{compressorSettings.showTarget}
    {
    }

    DynamicRangeProcessorSettings(const LimiterSettings& limiterSettings)
        : inCompressionThreshDb{limiterSettings.thresholdDb}
        , outCompressionThreshDb{limiterSettings.makeupTargetDb}
        , kneeWidthDb{limiterSettings.kneeWidthDb}
        , compressionRatio{std::numeric_limits<double>::infinity()}
        , lookaheadMs{limiterSettings.lookaheadMs}
        , attackMs{0.}
        , releaseMs{limiterSettings.releaseMs}
        , showInput{limiterSettings.showInput}
        , showOutput{limiterSettings.showOutput}
        , showActual{limiterSettings.showActual}
        , showTarget{limiterSettings.showTarget}
    {
    }

    double inCompressionThreshDb;
    double outCompressionThreshDb;
    double kneeWidthDb;
    double compressionRatio;
    double lookaheadMs;
    double attackMs;
    double releaseMs;
    double showInput;
    double showOutput;
    double showActual;
    double showTarget;
};

constexpr bool operator==(
    const DynamicRangeProcessorSettings& lhs,
    const DynamicRangeProcessorSettings& rhs)
{
    return lhs.inCompressionThreshDb == rhs.inCompressionThreshDb
           && lhs.outCompressionThreshDb == rhs.outCompressionThreshDb
           && lhs.kneeWidthDb == rhs.kneeWidthDb
           && lhs.compressionRatio == rhs.compressionRatio
           && lhs.lookaheadMs == rhs.lookaheadMs && lhs.attackMs == rhs.attackMs
           && lhs.releaseMs == rhs.releaseMs && lhs.showInput == rhs.showInput
           && lhs.showOutput == rhs.showOutput
           && lhs.showActual == rhs.showActual && lhs.showTarget == rhs.showTarget;
}

constexpr bool operator!=(
    const DynamicRangeProcessorSettings& lhs,
    const DynamicRangeProcessorSettings& rhs)
{
    return !(lhs == rhs);
}
