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
class LockFreeQueueNode;
struct PlayableSequence;

namespace au::au4audio {
class BlockContext;
class IEffectsContext;
class IEndOfTrackContext;
class IMuteSoloContext;

//! A track as a callback keeps it: its chain, by its last node, and what the callback needs of it
struct Track {
    //! What the mix reads; it owns the chain
    std::shared_ptr<ISequenceProcessingNode> output;
    //! What reads its chain ahead, if anything, in the chain
    std::shared_ptr<LockFreeQueueNode> queue;
    //! How far ahead of what plays the queue is read: by the latency of the effects after it
    int64_t queueLead = 0;
};

//! How a track renders its chain ahead, on a worker, behind a LockFreeQueueNode
struct TrackReadAhead {
    size_t chunkSize = 0;
    size_t numChunks = 0;
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
    //! If it renders its chain ahead
    std::optional<TrackReadAhead> readAhead;
    //! If it renders its chain ahead: what sets the state of its blocks, which must then be its effects, mute/solo and end
    //! of track contexts
    BlockContext* chainContext = nullptr;
    //! The master effects' latency, by which the master reads the chains ahead
    int masterLatency = 0;
};

//! The chain of `sequence`, read as it processes its clips: where the playhead reads, the end of the track, its effects,
//! mute/solo and volume/pan, ahead if it renders ahead
//! @param stop where the track stops, in the timeline, in samples, if it does
Track buildTrack(std::shared_ptr<const PlayableSequence> sequence, const TrackSettings& settings,
                 std::optional<int64_t> stop = std::nullopt);

}
