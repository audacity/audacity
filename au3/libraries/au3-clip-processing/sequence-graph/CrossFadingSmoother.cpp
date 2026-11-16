/*
 * Audacity: A Digital Audio Editor
 */
#include "CrossFadingSmoother.h"

#include <algorithm>
#include <cmath>
#include <numbers>

namespace {
std::vector<float> CreateFadeIn(size_t lookahead)
{
    std::vector<float> fade(lookahead);
    for (size_t i = 0; i < lookahead; ++i) {
        fade[i] = (1 - std::cos(std::numbers::pi * i / lookahead)) / 2;
    }
    return fade;
}
}

CrossFadingSmoother::CrossFadingSmoother(std::unique_ptr<ISequenceProcessingNode> input, size_t numChannels, size_t lookahead)
    : mInput{std::move(input)}
    , mLookahead{std::max<size_t>(lookahead, 1)}
    , mFadeIn{::CreateFadeIn(mLookahead)}
    , mInputSamples(numChannels, std::vector<float>(mLookahead))
    , mOutput(numChannels, std::vector<float>(mLookahead))
    , mReread(numChannels, std::vector<float>(mLookahead))
    , mPointers(numChannels)
{
}

float* const* CrossFadingSmoother::Pointers(ChannelBuffers& channelBuffers, size_t offset)
{
    for (size_t c = 0; c < channelBuffers.size(); ++c) {
        mPointers[c] = channelBuffers[c].data() + offset;
    }
    return mPointers.data();
}

void CrossFadingSmoother::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    // Allocates only when reads get longer than ever
    for (auto& channel : mInputSamples) {
        channel.resize(std::max(channel.size(), mLookahead + len));
    }
    for (auto& channel : mOutput) {
        channel.resize(std::max(channel.size(), mLookahead + len));
    }

    const auto lookahead = static_cast<sampleCount::type>(mLookahead);
    if (!mExpectedStart.has_value() || *mExpectedStart != start) {
        // New run: nothing to cross-fade from.
        mInput->GetSamples(Pointers(mInputSamples, 0), start, mLookahead);
        for (size_t c = 0; c < mInputSamples.size(); ++c) {
            std::copy_n(mInputSamples[c].begin(), mLookahead, mOutput[c].begin());
        }
    } else {
        // If the input's processing changed, these differ from what was read ahead
        mInput->GetSamples(Pointers(mReread, 0), start, mLookahead);
        auto changed = false;
        for (size_t c = 0; c < mReread.size() && !changed; ++c) {
            changed = !std::equal(mReread[c].begin(), mReread[c].end(), mInputSamples[c].begin());
        }
        if (changed) {
            for (size_t c = 0; c < mOutput.size(); ++c) {
                for (size_t i = 0; i < mLookahead; ++i) {
                    mOutput[c][i] = (1 - mFadeIn[i]) * mOutput[c][i] + mFadeIn[i] * mReread[c][i];
                }
                std::copy_n(mReread[c].begin(), mLookahead, mInputSamples[c].begin());
            }
        }
    }

    // Read further ahead
    mInput->GetSamples(Pointers(mInputSamples, mLookahead), start + lookahead, len);
    for (size_t c = 0; c < mInputSamples.size(); ++c) {
        std::copy_n(mInputSamples[c].begin() + mLookahead, len, mOutput[c].begin() + mLookahead);
    }

    for (size_t c = 0; c < mOutput.size(); ++c) {
        std::copy_n(mOutput[c].begin(), len, buffers[c]);
        // Keep what is now read ahead at the front
        std::copy_n(mInputSamples[c].begin() + len, mLookahead, mInputSamples[c].begin());
        std::copy_n(mOutput[c].begin() + len, mLookahead, mOutput[c].begin());
    }
    mExpectedStart = start + static_cast<sampleCount::type>(len);
}
