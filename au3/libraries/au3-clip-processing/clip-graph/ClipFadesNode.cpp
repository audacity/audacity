/*
 * Audacity: A Digital Audio Editor
 */
#include "ClipFadesNode.h"

ClipFadesNode::ClipFadesNode(std::unique_ptr<IClipProcessingNode> input, const ClipFades& fades, double rate)
    : mInput{std::move(input)}
    , mFades{fades}
    , mRate{rate}
{
}

void ClipFadesNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    mInput->GetSamples(buffers, start, len);
    // The fades may be changed while the clip is played, e.g. by dragging them during playback
    const auto fades = mFades.snapshot();
    const auto numChannels = NChannels();
    for (size_t i = 0; i < len; ++i) {
        const auto gain = fades.gainAt((start + static_cast<sampleCount::type>(i)).as_double() / mRate);
        if (gain == 1.0) {
            continue;
        }
        for (size_t c = 0; c < numChannels; ++c) {
            buffers[c][i] *= static_cast<float>(gain);
        }
    }
}

sampleCount ClipFadesNode::NumSamples() const
{
    return mInput->NumSamples();
}

size_t ClipFadesNode::NChannels() const
{
    return mInput->NChannels();
}
