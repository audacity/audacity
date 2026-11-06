/*
* Audacity: A Digital Audio Editor
*/
#include "tracktiontoaudacitynode.h"

#include <algorithm>

#include "../juce/stereo.h"
#include "audacitytotracktionnode.h"

using namespace au::au4tracktion;

TracktionToAudacityNode::TracktionToAudacityNode(std::vector<std::shared_ptr<ISequenceProcessingNode> > tracks, double sampleRate,
                                                 size_t maxFrames)
    : m_maxFrames{maxFrames}, m_buffer{au4juce::numChannels, static_cast<choc::buffer::FrameCount>(maxFrames)}
{
    size_t nodeId = 1;
    std::vector<std::unique_ptr<tracktion::graph::Node> > adapters;
    for (auto& track : tracks) {
        adapters.push_back(std::make_unique<AudacityToTracktionNode>(std::move(track), nodeId++));
    }
    // As tracktion_engine: a thread per CPU, the reading one included
    m_player = std::make_unique<tracktion::graph::LockFreeMultiThreadedNodePlayer>(
        tracktion::graph::getPoolCreatorFunction(tracktion::graph::ThreadPoolStrategy::lightweightSemHybrid));
    m_player->setNumThreads(static_cast<size_t>(std::max(0, juce::SystemStats::getNumCpus() - 1)));
    m_player->setNode(std::make_unique<tracktion::graph::SummingNode>(std::move(adapters)), sampleRate, static_cast<int>(maxFrames));
}

TracktionToAudacityNode::~TracktionToAudacityNode() = default;

void TracktionToAudacityNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    const auto index = start.as_long_long();
    for (size_t done = 0; done < len;) {
        const auto frames = std::min(len - done, m_maxFrames);
        const auto from = index + static_cast<int64_t>(done);
        auto view = m_buffer.getView().getStart(static_cast<choc::buffer::FrameCount>(frames));
        view.clear();
        m_midi.clear();
        m_player->process({ static_cast<choc::buffer::FrameCount>(frames), juce::Range<int64_t> { from, from + static_cast<int64_t>(frames) },
                            { view, m_midi } });
        for (int ch = 0; ch < au4juce::numChannels; ++ch) {
            std::copy_n(channelPointer(view, ch), frames, buffers[ch] + done);
        }
        done += frames;
    }
}
