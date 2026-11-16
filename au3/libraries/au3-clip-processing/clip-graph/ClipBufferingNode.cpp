/*
 * Audacity: A Digital Audio Editor
 */
#include "ClipBufferingNode.h"

#include <algorithm>

ClipBufferingNode::ClipBufferingNode(std::unique_ptr<IClipProcessingNode> input, size_t bufferLength)
    : mInput{std::move(input)}
    , mBuffer(mInput->NChannels(), std::vector<float>(bufferLength))
    , mInputBuffers(mInput->NChannels())
{
}

void ClipBufferingNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    const auto bufferStart = mExpectedStart ? *mExpectedStart - static_cast<sampleCount::type>(mNumBuffered) : start;
    if (!mExpectedStart || start < bufferStart || start > *mExpectedStart) {
        mExpectedStart = start;
        mNumBuffered = 0;
    }

    // What is buffered, ...
    const auto numFromBuffer = std::min(len, (*mExpectedStart - start).as_size_t());
    if (numFromBuffer > 0) {
        const auto bufferOffset = (start - (*mExpectedStart - static_cast<sampleCount::type>(mNumBuffered))).as_size_t();
        for (size_t c = 0; c < mBuffer.size(); ++c) {
            std::copy_n(mBuffer[c].begin() + bufferOffset, numFromBuffer, buffers[c]);
        }
    }
    const auto numFromInput = len - numFromBuffer;
    if (numFromInput == 0) {
        return;
    }
    // ... then the input, from where the previous read ended
    for (size_t c = 0; c < mBuffer.size(); ++c) {
        mInputBuffers[c] = buffers[c] + numFromBuffer;
    }
    mInput->GetSamples(mInputBuffers.data(), *mExpectedStart, numFromInput);
    mExpectedStart = start + static_cast<sampleCount::type>(len);

    // Buffer the last samples read
    const auto bufferLength = mBuffer.empty() ? 0 : mBuffer[0].size();
    const auto numNew = std::min(numFromInput, bufferLength);
    const auto numKept = std::min(mNumBuffered, bufferLength - numNew);
    for (size_t c = 0; c < mBuffer.size(); ++c) {
        std::copy(mBuffer[c].begin() + (mNumBuffered - numKept), mBuffer[c].begin() + mNumBuffered, mBuffer[c].begin());
        std::copy_n(mInputBuffers[c] + (numFromInput - numNew), numNew, mBuffer[c].begin() + numKept);
    }
    mNumBuffered = numKept + numNew;
}

sampleCount ClipBufferingNode::NumSamples() const
{
    return mInput->NumSamples();
}

size_t ClipBufferingNode::NChannels() const
{
    return mInput->NChannels();
}
