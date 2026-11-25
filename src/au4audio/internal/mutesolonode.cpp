/*
* Audacity: A Digital Audio Editor
*/
#include "mutesolonode.h"

#include <algorithm>

#include "au3-mixer/AudioIOSequences.h"

#include "imutesolocontext.h"
#include "stereo.h"

using namespace au::au4audio;

MuteSoloNode::MuteSoloNode(std::shared_ptr<ISequenceProcessingNode> input, MuteSoloSwitches switches,
                           const IMuteSoloContext& context, std::vector<std::function<bool()> > soloedWith)
    : m_input{std::move(input)}, m_switches{std::move(switches)}, m_context{context}, m_soloedWith{std::move(soloedWith)}
{
}

MuteSoloNode::MuteSoloNode(std::shared_ptr<ISequenceProcessingNode> input, std::shared_ptr<const PlayableSequence> track,
                           const IMuteSoloContext& context, std::vector<std::function<bool()> > soloedWith)
    : MuteSoloNode{std::move(input),
                   MuteSoloSwitches { [track] { return track->GetMute(); }, [track] { return track->GetSolo(); } },
                   context, std::move(soloedWith)}
{
}

void MuteSoloNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    m_input->GetSamples(buffers, start, len);
    // As AudioIO: a soloing track plays even if muted; one routed with a soloing track doesn't, if muted
    const bool soloedWith = std::any_of(m_soloedWith.begin(), m_soloedWith.end(), [](const auto& solo) { return solo(); });
    const bool silenced = !m_switches.solo() && (m_switches.mute() || (m_context.anySoloing() && !soloedWith));
    if (silenced) {
        for (int ch = 0; ch < numChannels; ++ch) {
            std::fill_n(buffers[ch], len, 0.f);
        }
    }
}
