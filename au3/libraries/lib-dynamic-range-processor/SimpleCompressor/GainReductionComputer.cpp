/*
 This file is part of the SimpleCompressor project.
 https://github.com/DanielRudrich/SimpleCompressor
 Copyright (c) 2019 Daniel Rudrich

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, version 3.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include "GainReductionComputer.h"
#include "MathApprox.h"

namespace DanielRudrich {
namespace {
float getSlope(float ratio) { return 1 / ratio - 1; }
float getRatio(float slope) { return 1 / (slope + 1); }
} // namespace

float GainReductionComputer::getCharacteristicSample(float inputLevelInDecibels,
                                                     float kneeInDecibels,
                                                     float thresholdInDecibels,
                                                     float ratio,
                                                     float makeUpGainInDecibels)
{
    const auto slope = getSlope(ratio);
    float overShoot = inputLevelInDecibels - thresholdInDecibels;
    overShoot = applyCharacteristicToOverShoot(overShoot, kneeInDecibels, slope);
    return overShoot + inputLevelInDecibels + makeUpGainInDecibels;
}

GainReductionComputer::GainReductionComputer()
{
    sampleRate = 0.0f;

    threshold = -10.0f;
    knee = 0.0f;
    kneeHalf = 0.0f;
    attackTime = 0.01f;
    releaseTime = 0.15f;
    setRatio(2);  // 2 : 1
    makeUpGain = 0.0f;
    reset();
}

void GainReductionComputer::prepare(const double newSampleRate)
{
    sampleRate = newSampleRate;

    alphaAttack = 1.0f - timeToGain(attackTime);
    alphaRelease = 1.0f - timeToGain(releaseTime);
}

void GainReductionComputer::setAttackTime(const float attackTimeInSeconds)
{
    attackTime = attackTimeInSeconds;
    alphaAttack = 1.0f - timeToGain(attackTime);
}

void GainReductionComputer::setReleaseTime(const float releaseTimeInSeconds)
{
    releaseTime = releaseTimeInSeconds;
    alphaRelease = 1.0f - timeToGain(releaseTime);
}

const float GainReductionComputer::timeToGain(const float timeInSeconds)
{
    return std::exp(-1.0f / (static_cast<float>(sampleRate) * timeInSeconds));
}

void GainReductionComputer::setKnee(const float kneeInDecibels)
{
    knee = kneeInDecibels;
    kneeHalf = knee / 2.0f;
}

void GainReductionComputer::setThreshold(const float thresholdInDecibels)
{
    threshold = thresholdInDecibels;
}

void GainReductionComputer::setMakeUpGain(const float makeUpGainInDecibels)
{
    makeUpGain = makeUpGainInDecibels;
}

void GainReductionComputer::setRatio(const float ratio)
{
    slope = getSlope(ratio);
}

inline const float GainReductionComputer::applyCharacteristicToOverShoot(const float overShootInDecibels)
{
    return applyCharacteristicToOverShoot(overShootInDecibels, knee, slope);
}

float GainReductionComputer::applyCharacteristicToOverShoot(
    float overShootInDecibels, float knee, float slope)
{
    const auto kneeHalf = knee / 2;
    if (overShootInDecibels <= -kneeHalf) {
        return 0.0f;
    } else if (overShootInDecibels > -kneeHalf && overShootInDecibels <= kneeHalf) {
        return 0.5f * slope * (overShootInDecibels + kneeHalf) * (overShootInDecibels + kneeHalf) / knee;
    } else {
        return slope * overShootInDecibels;
    }
}

void GainReductionComputer::computeGainInDecibelsFromSidechainSignal(const float* sideChainSignal, float* destination, const int numSamples)
{
    maxInputLevel = -std::numeric_limits<float>::infinity();
    maxGainReduction = 0.0f;

    for (int i = 0; i < numSamples; ++i) {
        // convert sample to decibels
        const float levelInDecibels
            =log2ToDb * FastLog2(std::abs(sideChainSignal[i]));

        if (levelInDecibels > maxInputLevel) {
            maxInputLevel = levelInDecibels;
        }

        // calculate overshoot and apply knee and ratio
        const float overShoot = levelInDecibels - threshold;
        const float gainReduction = applyCharacteristicToOverShoot(overShoot);

        // apply ballistics
        const float diff = gainReduction - state;
        if (diff < 0.0f) { // wanted gain reduction is below state -> attack phase
            state += alphaAttack * diff;
        } else { // release phase
            state += alphaRelease * diff;
        }

        // write back gain reduction
        destination[i] = state;

        if (state < maxGainReduction) {
            maxGainReduction = state;
        }
    }
}

void GainReductionComputer::computeLinearGainFromSidechainSignal(const float* sideChainSignal, float* destination, const int numSamples)
{
    computeGainInDecibelsFromSidechainSignal(sideChainSignal, destination, numSamples);
    for (int i = 0; i < numSamples; ++i) {
        destination[i] = std::pow(10.0f, 0.05f * (destination[i] + makeUpGain));
    }
}

void GainReductionComputer::getCharacteristic(float* inputLevelsInDecibels, float* dest, const int numSamples)
{
    for (int i = 0; i < numSamples; ++i) {
        dest[i] = getCharacteristicSample(inputLevelsInDecibels[i]);
    }
}

float GainReductionComputer::getCharacteristicSample(const float inputLevelInDecibels)
{
    return getCharacteristicSample(inputLevelInDecibels, knee, threshold,
                                   getRatio(slope), makeUpGain);
}
} // namespace DanielRudrich
