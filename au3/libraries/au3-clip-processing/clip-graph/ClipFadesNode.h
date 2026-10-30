/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ClipFades.h"
#include "IClipProcessingNode.h"

#include <memory>

//! Applies the fades of a clip to the samples it plays, after stretching
class ClipFadesNode final : public IClipProcessingNode
{
public:
    //! @param rate of the samples, as positions are in samples
    ClipFadesNode(std::unique_ptr<IClipProcessingNode> input, const ClipFades& fades, double rate);

    // IClipProcessingNode
    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;
    sampleCount NumSamples() const override;
    size_t NChannels() const override;

private:
    const std::unique_ptr<IClipProcessingNode> mInput;
    const ClipFades& mFades;
    const double mRate;
};
