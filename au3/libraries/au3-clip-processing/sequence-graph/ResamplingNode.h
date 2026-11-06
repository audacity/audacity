/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-mixer/ISequenceProcessingNode.h"

#include "au3-math/Resample.h"

#include <memory>
#include <optional>
#include <vector>

//! Resamples its input to another rate
/*!
 * Positions are at the output rate. Reading elsewhere than where the previous read ended restarts the resampling there.
 * Construct it on the main thread: it reads the resampling method from the preferences.
 */
class ResamplingNode final : public ISequenceProcessingNode
{
public:
    ResamplingNode(std::shared_ptr<ISequenceProcessingNode> input, size_t numChannels, double inputRate, double outputRate);
    ~ResamplingNode() override;

    // ISequenceProcessingNode
    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

private:
    void Restart(sampleCount start);

    const std::shared_ptr<ISequenceProcessingNode> mInput;
    const size_t mNumChannels;
    const double mInputRate;
    const double mOutputRate;

    std::vector<Resample> mResamplers;
    //! Per channel, input samples not consumed by the resampler yet, from `mInputStart`
    std::vector<std::vector<float> > mInputBuffers;
    std::vector<float*> mInputPointers;
    size_t mInputStart = 0;
    size_t mInputLen = 0;
    //! Where the next input read starts
    sampleCount mInputPosition = 0;
    //! Where the next output read starts if it continues the previous one
    std::optional<sampleCount> mExpectedStart;
};
