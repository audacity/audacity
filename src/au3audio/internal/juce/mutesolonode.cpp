/*
* Audacity: A Digital Audio Editor
*/
#include "mutesolonode.h"

#include <algorithm>

#include "au3-mixer/AudioIOSequences.h"

#include "imutesolocontext.h"
#include "stereo.h"

using namespace au::au4juce;

MuteSoloNode::MuteSoloNode(std::shared_ptr<ISequenceProcessingNode> input, std::shared_ptr<const PlayableSequence> track,
                           const IMuteSoloContext& context)
    : m_input{std::move(input)}, m_track{std::move(track)}, m_context{context}
{
}

void MuteSoloNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    m_input->GetSamples(buffers, start, len);
    // As AudioIO: a soloing track plays even if muted
    const bool silenced = !m_track->GetSolo() && (m_context.anySoloing() || m_track->GetMute());
    if (silenced) {
        for (int ch = 0; ch < numChannels; ++ch) {
            std::fill_n(buffers[ch], len, 0.f);
        }
    }
}
