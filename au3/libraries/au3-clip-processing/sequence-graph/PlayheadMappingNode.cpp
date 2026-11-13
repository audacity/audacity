/*
 * Audacity: A Digital Audio Editor
 */
#include "PlayheadMappingNode.h"

#include <algorithm>
#include <cassert>

PlayheadMappingNode::PlayheadMappingNode(std::shared_ptr<ISequenceProcessingNode> input,
                                         std::shared_ptr<const IPlayheadMapper> mapper, size_t numChannels)
    : mInput{std::move(input)}
    , mMapper{std::move(mapper)}
    , mBuffers(numChannels)
{
}

void PlayheadMappingNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    size_t done = 0;
    while (done < len) {
        const auto segment = mMapper->Map(start + done, len - done);
        assert(segment.len > 0);
        if (segment.len == 0) {
            for (size_t i = 0; i < mBuffers.size(); ++i) {
                std::fill_n(buffers[i] + done, len - done, 0.f);
            }
            return;
        }
        for (size_t i = 0; i < mBuffers.size(); ++i) {
            mBuffers[i] = buffers[i] + done;
        }
        mInput->GetSamples(mBuffers.data(), segment.position, segment.len);
        done += segment.len;
    }
}
