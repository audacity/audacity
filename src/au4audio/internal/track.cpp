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
#include "monitoredinput.h"
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
    std::shared_ptr<LockFreeQueueNode> queue;
    const auto& readAhead = settings.readAhead;
    // The device waits for the workers: after a seek, they resume where the device is
    constexpr size_t resyncLead = 0;
    if (readAhead && !readAhead->wholeChain) {
        queue = std::make_shared<LockFreeQueueNode>(std::move(node), numSequenceChannels, readAhead->chunkSize, readAhead->numChunks,
                                                    resyncLead);
        node = queue;
    }
    node = std::make_shared<EndOfTrackNode>(std::move(node), settings.mapper, *settings.endOfTrack, stop);
    const auto group = sequence->FindChannelGroup();
    if (settings.monitoredInput && group) {
        // Read by the track's effects ahead by their latency, as the master reads the track ahead by its
        const auto lead = settings.masterLatency + settings.effects->latency(group);
        auto input = std::make_shared<MonitoredInputNode>(settings.monitoredInput, numSequenceChannels, lead);
        std::vector<std::shared_ptr<ISequenceProcessingNode> > sources { std::move(node), std::move(input) };
        node = std::make_shared<SequentialMixNode>(std::move(sources), settings.maxFrames);
    }
    // Without a group, it has no effects; a null group would be the master's
    if (group) {
        node = std::make_shared<EffectsNode>(std::move(node), numSequenceChannels, group, *settings.effects, settings.maxFrames);
    }
    node = std::make_shared<MuteSoloNode>(std::move(node), sequence, *settings.muteSolo);
    node = std::make_shared<GainPanNode>(std::move(node), sequence);
    if (readAhead && readAhead->wholeChain) {
        // The tap is read where the device is: after the queue, which is stereo, as volume/pan outputs
        node = std::make_shared<BlockContextNode>(std::move(node), *settings.chainContext);
        queue = std::make_shared<LockFreeQueueNode>(std::move(node), numChannels, readAhead->chunkSize, readAhead->numChunks,
                                                    resyncLead);
        node = queue;
    }
    auto tap = std::make_shared<MeterTapNode>(std::move(node), numSequenceChannels, settings.meterCapacity);
    std::optional<int64_t> meterId;
    if (const auto waveTrack = dynamic_cast<const WaveTrack*>(sequence->FindChannelGroup())) {
        meterId = static_cast<int64_t>(waveTrack->GetId());
    }
    // The master's effects read the tracks ahead by their latency, and the track's effects their input by theirs
    int64_t queueLead = 0;
    if (queue) {
        queueLead = settings.masterLatency + (readAhead->wholeChain || !group ? 0 : settings.effects->latency(group));
    }
    return { tap, tap, std::move(queue), queueLead, meterId, std::min<size_t>(numSequenceChannels, numChannels) };
}

std::shared_ptr<ISequenceProcessingNode> au::au4audio::buildMonitoredInput(std::shared_ptr<const PlayableSequence> track,
                                                                           const TrackSettings& settings)
{
    const auto group = track ? track->FindChannelGroup() : nullptr;
    if (!group) {
        return std::make_shared<MonitoredInputNode>(settings.monitoredInput, numChannels, settings.masterLatency);
    }
    const auto numTrackChannels = track->NChannels();
    std::shared_ptr<ISequenceProcessingNode> node = std::make_shared<MonitoredInputNode>(
        settings.monitoredInput, numTrackChannels, settings.masterLatency + settings.effects->latency(group));
    node = std::make_shared<EffectsNode>(std::move(node), numTrackChannels, group, *settings.effects, settings.maxFrames);
    node = std::make_shared<MuteSoloNode>(std::move(node), track, *settings.muteSolo);
    return std::make_shared<GainPanNode>(std::move(node), track);
}
