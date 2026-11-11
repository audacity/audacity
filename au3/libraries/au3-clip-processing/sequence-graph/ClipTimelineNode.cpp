/*
 * Audacity: A Digital Audio Editor
 */
#include "ClipTimelineNode.h"
#include "ClipInterface.h"

#include <algorithm>

ClipTimelineNode::ClipTimelineNode(std::vector<ClipAndGraph> clips, size_t numChannels, double rate)
    : mClips{std::move(clips)}
    , mRate{rate}
    , mNumChannels{numChannels}
    , mOffsetBuffers(numChannels)
{
}

void ClipTimelineNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    // Silence where there are no clips
    for (size_t c = 0; c < mNumChannels; ++c) {
        std::fill_n(buffers[c], len, 0.f);
    }

    const auto end = start + static_cast<sampleCount::type>(len);
    for (const auto& [clip, graph] : mClips) {
        const auto clipFirst = sampleCount { clip->GetPlayStartTime() * mRate + .5 };
        const auto clipEnd = clipFirst + graph->NumSamples();
        const auto from = std::max(start, clipFirst);
        const auto to = std::min(end, clipEnd);
        if (from >= to) {
            continue;
        }
        for (size_t c = 0; c < mNumChannels; ++c) {
            mOffsetBuffers[c] = buffers[c] + (from - start).as_size_t();
        }
        graph->GetSamples(mOffsetBuffers.data(), from - clipFirst, (to - from).as_size_t());
    }
}
