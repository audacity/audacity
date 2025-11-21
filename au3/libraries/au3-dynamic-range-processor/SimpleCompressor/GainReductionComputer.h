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

#pragma once

#include <vector>
#include <limits>
#include <cmath>
#include <atomic>

namespace DanielRudrich {
/**
 This class acts as the side-chain path of a dynamic range compressor. It processes a given side-chain signal and computes the gain reduction samples depending on the parameters threshold, knee, attack-time, release-time, ratio, and make-up gain.
 */
class GainReductionComputer
{
public:
    GainReductionComputer();
    ~GainReductionComputer() {}

    static float getCharacteristicSample(float inputLevelInDecibels, float kneeInDecibels, float thresholdInDecibels, float ratio,
                                         float makeUpGainInDecibels);

    // ======================================================================
    /**
     Sets the attack time of the compressor in seconds.
     */
    void setAttackTime(const float attackTimeInSeconds);

    /**
     Sets the release time of the compressorin seconds
     */
    void setReleaseTime(const float releaseTimeInSeconds);

    /**
     Sets the knee-width in decibels.
     */
    void setKnee(const float kneeInDecibels);
    const float getKnee() { return knee; }

    /**
     Sets the threshold above which the compressor will start to compress the signal.
     */
    void setThreshold(const float thresholdInDecibels);
    const float getThreshold() { return threshold; }

    /**
     Sets the make-up-gain of the compressor in decibels.
     */
    void setMakeUpGain(const float makeUpGainInDecibels);
    const float getMakeUpGain() { return makeUpGain; }

    /**
     Sets the ratio of input-output signal above threshold. Set to 1 for no compression, up to infinity for a brickwall limiter.
     */
    void setRatio(const float ratio);

    // ======================================================================
    /**
     Computes the static output levels for an array of input levels in decibels. Useful for visualization of the compressor's characteristic. Will contain make-up gain.
     */
    void getCharacteristic(float* inputLevelsInDecibels, float* destination, const int numSamples);

    /**
     Computes the static output levels for a given input level in decibels. Useful for visualization of the compressor's characteristic. Will contain make-up gain.
     */
    float getCharacteristicSample(const float inputLevelInDecibels);

    // ======================================================================
    /**
     Prepares the compressor with sampleRate and expected blockSize. Make sure you call this before you do any processing!
     */
    void prepare(const double sampleRate);

    /**
     Resets the internal state of the compressor.
     */
    void reset() { state = 0.0f; }

    /**
     Computes the gain reduction for a given side-chain signal. The values will be in decibels and will NOT contain the make-up gain.
     */
    void computeGainInDecibelsFromSidechainSignal(const float* sideChainSignal, float* destination, const int numSamples);

    /**
     Computes the linear gain including make-up gain for a given side-chain signal. The gain written to the destination can be directly applied to the signals which should be compressed.
     */
    void computeLinearGainFromSidechainSignal(const float* sideChainSignal, float* destination, const int numSamples);

    const float getMaxInputLevelInDecibels() { return maxInputLevel; }
    const float getMaxGainReductionInDecibels() { return maxGainReduction; }

private:
    static float applyCharacteristicToOverShoot(float overShootInDecibels, float knee, float slope);

    inline const float timeToGain(const float timeInSeconds);
    inline const float applyCharacteristicToOverShoot(const float overShootInDecibels);

    double sampleRate;

    // parameters
    float knee, kneeHalf;
    float threshold;
    float attackTime;
    float releaseTime;
    float slope;
    float makeUpGain;

    std::atomic<float> maxInputLevel { -std::numeric_limits<float>::infinity() };
    std::atomic<float> maxGainReduction { 0 };

    //state variable
    float state;

    float alphaAttack;
    float alphaRelease;
};
} // namespace DanielRudrich
