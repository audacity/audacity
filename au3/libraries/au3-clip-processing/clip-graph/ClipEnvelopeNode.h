/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "IClipProcessingNode.h"

#include <memory>
#include <vector>

class ClipInterface;

//! Applies the gain envelope of a clip to the samples it plays, after stretching
class ClipEnvelopeNode final : public IClipProcessingNode
{
public:
    //! @param rate of the samples, as positions are in samples
    ClipEnvelopeNode(std::unique_ptr<IClipProcessingNode> input, const ClipInterface& clip, double rate);

    // IClipProcessingNode
    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;
    sampleCount NumSamples() const override;
    size_t NChannels() const override;

private:
    const std::unique_ptr<IClipProcessingNode> mInput;
    const ClipInterface& mClip;
    const double mRate;
    std::vector<double> mValues;
};
