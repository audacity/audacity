/*
* Audacity: A Digital Audio Editor
*/
#include "track.h"

#include "au3-clip-processing/sequence-graph/LockFreeQueueNode.h"
#include "au3-clip-processing/sequence-graph/PlayheadMappingNode.h"
#include "au3-clip-processing/sequence-graph/ResamplingNode.h"
#include "au3-mixer/AudioIOSequences.h"
#include "au3-wave-track/WaveTrack.h"

#include "blockcontext.h"
#include "effectsnode.h"
#include "endoftracknode.h"
#include "gainpannode.h"
#include "ieffectscontext.h"
#include "metertapnode.h"
#include "mutesolonode.h"
#include "sequentialmixnode.h"
#include "stereo.h"

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

// au3's ::Track is visible too
au::au4audio::Track au::au4audio::buildTrack(std::shared_ptr<const PlayableSequence> sequence, const TrackSettings& settings,
                                             std::optional<int64_t> stop)
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
    node = std::make_shared<GainPanNode>(std::move(node), sequence);
    std::shared_ptr<LockFreeQueueNode> queue;
    if (const auto& readAhead = settings.readAhead) {
        // The tap is read where the device is: after the queue, which is stereo, as volume/pan outputs. The device waits for
        // the workers: after a seek, they resume where the device is.
        node = std::make_shared<BlockContextNode>(std::move(node), *settings.chainContext);
        constexpr size_t resyncLead = 0;
        queue = std::make_shared<LockFreeQueueNode>(std::move(node), numChannels, readAhead->chunkSize, readAhead->numChunks,
                                                    resyncLead);
        node = queue;
    }
    auto tap = std::make_shared<MeterTapNode>(std::move(node), numSequenceChannels, settings.meterCapacity);
    std::optional<int64_t> meterId;
    if (const auto waveTrack = dynamic_cast<const WaveTrack*>(sequence->FindChannelGroup())) {
        meterId = static_cast<int64_t>(waveTrack->GetId());
    }
    // The master's effects read the tracks ahead by their latency
    const int64_t queueLead = queue ? settings.masterLatency : 0;
    return { tap, tap, std::move(queue), queueLead, meterId, std::min<size_t>(numSequenceChannels, numChannels) };
}
