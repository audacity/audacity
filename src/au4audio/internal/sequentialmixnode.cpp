/*
* Audacity: A Digital Audio Editor
*/
#include "sequentialmixnode.h"

#include <algorithm>

using namespace au::au4audio;

SequentialMixNode::SequentialMixNode(std::vector<std::shared_ptr<ISequenceProcessingNode> > tracks, size_t maxFrames)
    : m_tracks{std::move(tracks)}, m_maxFrames{maxFrames}
{
    for (int ch = 0; ch < numChannels; ++ch) {
        m_track[ch].assign(maxFrames, 0.f);
        m_trackPointers[ch] = m_track[ch].data();
    }
}

void SequentialMixNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    for (int ch = 0; ch < numChannels; ++ch) {
        std::fill_n(buffers[ch], len, 0.f);
    }
    for (size_t done = 0; done < len;) {
        const auto frames = std::min(len - done, m_maxFrames);
        for (const auto& track : m_tracks) {
            track->GetSamples(m_trackPointers, start + done, frames);
            for (int ch = 0; ch < numChannels; ++ch) {
                auto* const output = buffers[ch] + done;
                for (size_t i = 0; i < frames; ++i) {
                    output[i] += m_track[ch][i];
                }
            }
        }
        done += frames;
    }
}
