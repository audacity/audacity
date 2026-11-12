/*
* Audacity: A Digital Audio Editor
*/
#include "track.h"

#include "au3-clip-processing/sequence-graph/PlayheadMappingNode.h"
#include "au3-clip-processing/sequence-graph/ResamplingNode.h"
#include "au3-mixer/AudioIOSequences.h"

#include "effectsnode.h"
#include "endoftracknode.h"
#include "gainpannode.h"
#include "mutesolonode.h"

using namespace au::au4audio;

namespace {
//! Reads a sequence, which reads the graph processing its clips (see PlayableSequenceFactory): forward only
class SequenceReadingNode final : public ISequenceProcessingNode
{
public:
    explicit SequenceReadingNode(std::shared_ptr<const PlayableSequence> sequence)
        : m_sequence{std::move(sequence)} {}

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override
    {
        m_sequence->GetFloats(0, m_sequence->NChannels(), buffers, start, len);
    }

private:
    const std::shared_ptr<const PlayableSequence> m_sequence;
};
}

Track au::au4audio::buildTrack(std::shared_ptr<const PlayableSequence> sequence, const TrackSettings& settings, std::optional<int64_t> stop)
{
    const auto numSequenceChannels = sequence->NChannels();
    std::shared_ptr<ISequenceProcessingNode> node = std::make_shared<SequenceReadingNode>(sequence);
    if (sequence->GetRate() != settings.sampleRate) {
        node = std::make_shared<ResamplingNode>(std::move(node), numSequenceChannels, sequence->GetRate(), settings.sampleRate);
    }
    node = std::make_shared<PlayheadMappingNode>(std::move(node), settings.mapper, numSequenceChannels);
    node = std::make_shared<EndOfTrackNode>(std::move(node), settings.mapper, *settings.endOfTrack, stop);
    // Without a group, it has no effects; a null group would be the master's
    if (const auto group = sequence->FindChannelGroup()) {
        node = std::make_shared<EffectsNode>(std::move(node), numSequenceChannels, group, *settings.effects, settings.maxFrames);
    }
    node = std::make_shared<MuteSoloNode>(std::move(node), sequence, *settings.muteSolo);
    return { std::make_shared<GainPanNode>(std::move(node), sequence) };
}
