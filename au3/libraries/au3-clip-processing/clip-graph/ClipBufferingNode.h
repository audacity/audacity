/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "IClipProcessingNode.h"

#include <memory>
#include <optional>
#include <vector>

//! Keeps the last samples read from its input, so that they can be read again without reading the input again
/*!
 * E.g. a stateful input, like stretching, which reading elsewhere than where the previous read ended would restart.
 */
class ClipBufferingNode final : public IClipProcessingNode
{
public:
    //! @param bufferLength how many of the last samples read can be read again
    ClipBufferingNode(std::unique_ptr<IClipProcessingNode> input, size_t bufferLength);

    // IClipProcessingNode
    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;
    sampleCount NumSamples() const override;
    size_t NChannels() const override;

private:
    const std::unique_ptr<IClipProcessingNode> mInput;
    //! Where the previous read ended
    std::optional<sampleCount> mExpectedStart;
    //! Per channel, the last samples read, up to `mExpectedStart`, oldest first
    std::vector<std::vector<float> > mBuffer;
    size_t mNumBuffered = 0;
    std::vector<float*> mInputBuffers;
};
