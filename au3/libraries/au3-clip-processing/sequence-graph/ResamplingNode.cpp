/*
 * Audacity: A Digital Audio Editor
 */
#include "ResamplingNode.h"

#include <cmath>

namespace {
//! How many input samples are read at once
constexpr size_t inputBlockSize = 1024;
}

ResamplingNode::ResamplingNode(std::shared_ptr<ISequenceProcessingNode> input, size_t numChannels, double inputRate,
                               double outputRate)
    : mInput{std::move(input)}
    , mNumChannels{numChannels}
    , mInputRate{inputRate}
    , mOutputRate{outputRate}
    , mInputBuffers(numChannels, std::vector<float>(inputBlockSize))
    , mInputPointers(numChannels)
{
    // Here rather than when restarting, as it reads preferences
    const auto factor = mOutputRate / mInputRate;
    for (size_t i = 0; i < mNumChannels; ++i) {
        // The fast method, as the Mixer uses for playback
        mResamplers.emplace_back(false, factor, factor);
    }
}

ResamplingNode::~ResamplingNode() = default;

void ResamplingNode::Restart(sampleCount start)
{
    for (auto& resampler : mResamplers) {
        resampler.Reset();
    }
    mInputStart = 0;
    mInputLen = 0;
    mInputPosition = sampleCount { std::llround(start.as_double() * mInputRate / mOutputRate) };
}

void ResamplingNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    if (mExpectedStart != start) {
        Restart(start);
    }
    mExpectedStart = start + len;

    const auto factor = mOutputRate / mInputRate;
    size_t produced = 0;
    while (produced < len) {
        if (mInputLen == 0) {
            for (size_t i = 0; i < mNumChannels; ++i) {
                mInputPointers[i] = mInputBuffers[i].data();
            }
            mInput->GetSamples(mInputPointers.data(), mInputPosition, inputBlockSize);
            mInputPosition += inputBlockSize;
            mInputStart = 0;
            mInputLen = inputBlockSize;
        }
        std::pair<size_t, size_t> result;
        for (size_t i = 0; i < mNumChannels; ++i) {
            // The resamplers progress alike, as they get the same amounts of samples
            result = mResamplers[i].Process(factor, mInputBuffers[i].data() + mInputStart, mInputLen, false,
                                            buffers[i] + produced, len - produced);
        }
        mInputStart += result.first;
        mInputLen -= result.first;
        produced += result.second;
    }
}
