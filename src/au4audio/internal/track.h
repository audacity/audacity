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
class MeterTapNode;
class MonitoredInput;

//! A track as a callback keeps it: its chain, by its last node, and what the callback needs of it
struct Track {
    //! What the mix reads; it owns the chain
    std::shared_ptr<ISequenceProcessingNode> output;
    //! Its meter's tap, in the chain
    std::shared_ptr<MeterTapNode> tap;
    //! What reads its sequence, or its whole chain, ahead, if anything, in the chain
    std::shared_ptr<LockFreeQueueNode> queue;
    //! How far ahead of what plays the queue is read: by the latency of the effects after it
    int64_t queueLead = 0;
    //! Its meter's, if it has one
    std::optional<int64_t> meterId;
    //! How many channels its meter shows
    size_t numMeterChannels = 0;
};

//! How a track reads its sequence ahead, on a worker, behind a LockFreeQueueNode
struct TrackReadAhead {
    size_t chunkSize = 0;
    size_t numChunks = 0;
    //! Whether the worker renders the whole chain but its meter's tap, rather than reading the sequence only
    bool wholeChain = false;
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
    //! If it reads its sequence ahead
    std::optional<TrackReadAhead> readAhead;
    //! If it reads its whole chain ahead: what sets the state of its blocks, which must then be its effects, mute/solo
    //! and end of track contexts
    BlockContext* chainContext = nullptr;
    //! If the input is monitored through the chain's effects; then the master effects' latency, by which the master reads
    //! the chains ahead
    std::shared_ptr<const MonitoredInput> monitoredInput;
    int masterLatency = 0;
};

//! The chain of `sequence`, read as it processes its clips: where the playhead reads, ahead if it reads ahead,
//! the end of the track, its effects, mute/solo, volume/pan and meter
//! @param stop where the track stops, in the timeline, in samples, if it does
//! If `settings` has a monitored input, the track is monitored: the input joins what it reads, before its effects.
Track buildTrack(std::shared_ptr<const PlayableSequence> sequence, const TrackSettings& settings,
                 std::optional<int64_t> stop = std::nullopt);

//! The monitored input of `settings`, as the mix sums it, through `track`, which isn't played: its effects, mute/solo and
//! volume/pan; as it comes if null, the master's effects following
std::shared_ptr<ISequenceProcessingNode> buildMonitoredInput(std::shared_ptr<const PlayableSequence> track, const TrackSettings& settings);
}
