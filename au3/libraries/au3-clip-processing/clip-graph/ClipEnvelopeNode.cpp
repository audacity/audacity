/*
 * Audacity: A Digital Audio Editor
 */
#include "ClipEnvelopeNode.h"

#include "ClipInterface.h"

#include "au3-mixer/Envelope.h"

#include <algorithm>

ClipEnvelopeNode::ClipEnvelopeNode(std::unique_ptr<IClipProcessingNode> input, const ClipInterface& clip, double rate)
    : mInput{std::move(input)}
    , mClip{clip}
    , mRate{rate}
{
}

void ClipEnvelopeNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    mInput->GetSamples(buffers, start, len);
    // The envelope may be changed while the clip is played
    const auto& envelope = mClip.GetEnvelope();
    if (envelope.IsTrivial()) {
        return;
    }
    // Allocates only when reads get longer than ever
    mValues.resize(std::max(mValues.size(), len));
    // The envelope is in the time of the track
    const auto t0 = mClip.GetPlayStartTime() + start.as_double() / mRate;
    envelope.GetValues(mValues.data(), static_cast<int>(len), t0, 1.0 / mRate);
    const auto numChannels = NChannels();
    for (size_t c = 0; c < numChannels; ++c) {
        for (size_t i = 0; i < len; ++i) {
            buffers[c][i] *= static_cast<float>(mValues[i]);
        }
    }
}

sampleCount ClipEnvelopeNode::NumSamples() const
{
    return mInput->NumSamples();
}

size_t ClipEnvelopeNode::NChannels() const
{
    return mInput->NChannels();
}
