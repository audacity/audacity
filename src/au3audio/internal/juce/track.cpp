/*
* Audacity: A Digital Audio Editor
*/
#include "track.h"

#include "au3-clip-processing/sequence-graph/PlayheadMappingNode.h"
#include "au3-clip-processing/sequence-graph/ResamplingNode.h"
#include "au3-mixer/AudioIOSequences.h"
#include "au3-wave-track/WaveTrack.h"

#include "effectsnode.h"
#include "endoftracknode.h"
#include "gainpannode.h"
#include "metertapnode.h"
#include "mutesolonode.h"
#include "stereo.h"

using namespace au::au4juce;

// au3's ::Track is visible too
au::au4juce::Track au::au4juce::buildTrack(std::shared_ptr<const PlayableSequence> sequence, std::shared_ptr<ISequenceProcessingNode> graph,
                              const TrackSettings& settings, std::optional<int64_t> stop)
{
    const auto numSequenceChannels = sequence->NChannels();
    auto node = std::move(graph);
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
    auto tap = std::make_shared<MeterTapNode>(std::move(node), numSequenceChannels, settings.meterCapacity);
    std::optional<int64_t> meterId;
    if (const auto waveTrack = dynamic_cast<const WaveTrack*>(sequence->FindChannelGroup())) {
        meterId = static_cast<int64_t>(waveTrack->GetId());
    }
    return { tap, tap, meterId, std::min<size_t>(numSequenceChannels, numChannels) };
}
