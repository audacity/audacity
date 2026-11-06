/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>

class IPlayheadMapper;
class ISequenceProcessingNode;
class PlayableSequence;

namespace au::au4juce {
class IEffectsContext;
class IEndOfTrackContext;
class IMuteSoloContext;
class MeterTapNode;

//! A track as a callback keeps it: its chain, by its last node, and what the callback needs of it
struct Track {
    //! What the mix reads; it owns the chain
    std::shared_ptr<ISequenceProcessingNode> output;
    //! Its meter's tap, in the chain
    std::shared_ptr<MeterTapNode> tap;
};

//! What a track's chain needs besides its sequence
struct TrackSettings {
    std::shared_ptr<const IPlayheadMapper> mapper;
    IEffectsContext* effects = nullptr;
    IMuteSoloContext* muteSolo = nullptr;
    IEndOfTrackContext* endOfTrack = nullptr;
    //! The rate of the stream
    double sampleRate = 0.0;
    //! The most the chain renders at once, as the effects are initialized for
    size_t maxFrames = 0;
    //! The most its meter's tap keeps: what a block reads of it
    size_t meterCapacity = 0;
};

//! The chain of `sequence`, read from `graph`, its clip processing: where the playhead reads, the end of the track, its
//! effects, mute/solo, volume/pan and meter
//! @param stop where the track stops, in the timeline, in samples, if it does
Track buildTrack(std::shared_ptr<const PlayableSequence> sequence, std::shared_ptr<ISequenceProcessingNode> graph,
                 const TrackSettings& settings, std::optional<int64_t> stop = std::nullopt);
}
