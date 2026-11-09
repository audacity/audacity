/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <atomic>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "audio/iaudioengine.h"

#include "au3-mixer/AudioIOSequences.h"
#include "au3-realtime-effects/RealtimeEffectManager.h"
#include "au3-utility/LockFreeQueue.h"

#include "audio/idevice.h"
#include "playedrange.h"
#include "playheadmapper.h"
#include "stereo.h"
#include "track.h"

class AudacityProject;
class ISequenceProcessingNode;

namespace au::au4audio {
class BlockContext;
class MeterSink;
class RecordingCapture;
class MeterTapNode;

//! A stream on a device: what it plays, and what it records if it does
/*!
 * It builds the tracks' chains and the master's, plays them as the playhead says, records, applies the playback volume,
 * and reports what plays and what the meters show.
 *
 * Each track's chain renders ahead of the device on a worker of its own, behind its own LockFreeQueueNode: peaks of
 * processing are absorbed, and the tracks' effects render in parallel. The mix and the master's effects render in the
 * device callback. The device waits for the workers: if one hasn't rendered in time, everything plays late, after
 * silence.
 */
class IoCallback final : public audio::IDeviceCallback
{
public:
    //! Reports what it plays to `callbackInfos` and `playedRanges`, and applies `playbackVolume`. The tracks render
    //! `readAheadDuration` ahead of the device, in seconds, and at least a device block.
    IoCallback(AudacityProject& project, int token, std::unique_ptr<audio::IDevice> device, bool followsPlayRegion,
               LockFreeQueue<audio::AudioCallbackInfo>& callbackInfos, LockFreeQueue<PlayedRange>& playedRanges,
               const std::atomic<float>& playbackVolume, double readAheadDuration);
    ~IoCallback() override;

    double sampleRate() const { return m_sampleRate; }

    //! Where a recording starts and ends, in the timeline, in seconds
    struct RecordingRange {
        double start = 0.0;
        double end = 0.0;
    };

    //! The tracks and the master's chain, for `sequences`, sending to `meter`, if any, what they play. If recording, as
    //! AudioIO, the tracks stop playing where the recording ends, and those recorded into with a lead-in where it starts.
    void build(const TransportSequences& sequences, const audio::IAudioMeterPtr& meter, std::optional<RecordingRange> recording);
    //! Records the input; call before start()
    void setCapture(std::unique_ptr<RecordingCapture> capture);
    const RecordingCapture* capture() const { return m_capture.get(); }
    //! Plays from `startTime` to `endTime`, or loops `loopRange` if given, rolling into it if `startTime` is left of it;
    //! then starts rendering ahead
    void setPlayRange(double startTime, double endTime, std::optional<std::pair<double, double> > loopRange);
    //! Main thread, while playing: the play region changed
    void updateLoop(std::optional<std::pair<double, double> > loopRange, double endTime);

    //! Where in the timeline the device is
    double position() const { return toTime(m_mapper->position()); }
    void seek(double time);
    void setPaused(bool paused) { m_paused = paused; }
    bool isFinished() const { return m_finished; }

    void start();
    //! Once stopped, it doesn't start again
    void stop();

    RealtimeEffects::InitializationScope* effectsInitialization();

    // IDeviceCallback
    void process(const float* const* input, int numInputChannels, float* const* output, int numOutputChannels, int frames) override;

    AudacityProject& project;
    const int token;
    //! Whether loop play follows the play region, as with the default playback policy
    const bool followsPlayRegion;
    double trackEndTime = 0.0;

private:
    //! What the tracks render ahead is rendered in chunks of this duration
    static constexpr double readAheadChunkDuration = 0.01;

    int64_t toSamples(double time) const { return static_cast<int64_t>(std::llround(time * m_sampleRate)); }
    double toTime(int64_t samples) const { return samples / m_sampleRate; }

    //! Once the playhead is set
    void startReadingAhead();
    //! Any thread, after the playhead changed what it plays ahead of the device: what was rendered ahead isn't played
    void invalidateReadAhead();
    //! Any thread: the stream index that what was read ahead, or is being read, ends at, as the sequences are read
    int64_t readAheadEnd() const;
    //! Device thread: renders the `frames` frames of the stream from `index` into `stereo`. Returns how many, from the
    //! first, the workers had all rendered: the stream advances by as many.
    size_t render(float* const* stereo, int64_t index, size_t frames);
    //! Device thread: plays `frames` frames from `offset` on, at most the block size. Returns how many, at the end, it
    //! couldn't play, being silent instead: the transport doesn't advance by them.
    int renderChunk(float* const* output, int numOutputChannels, int offset, int frames, audio::TimePoint dacTime);
    //! `stereo` goes out with the playback volume; a mono device gets the downmix
    void writeOutput(const float* const* stereo, float* const* output, int numOutputChannels, int offset, int frames) const;
    void reportPlayed(int64_t start, int64_t frames);

    //! At least a sample long
    PlayheadMapper::Range toLoopRange(const std::pair<double, double>& loopRange) const;
    //! What was rendered ahead isn't played, nor shown
    void invalidate();
    //! What of the timeline the device plays from `from` to `end`, segment by segment
    std::vector<std::pair<int64_t, int64_t> > segments(int64_t from, int64_t end) const;

    const double m_sampleRate;
    const std::weak_ptr<AudacityProject> m_wProject;
    const int m_blockSize;
    const size_t m_readAheadChunkSize;
    const size_t m_readAheadNumChunks;
    LockFreeQueue<audio::AudioCallbackInfo>& m_callbackInfos;
    LockFreeQueue<PlayedRange>& m_playedRanges;
    const std::atomic<float>& m_playbackVolume;

    std::unique_ptr<audio::IDevice> m_device;
    std::unique_ptr<RecordingCapture> m_capture;

    const std::shared_ptr<PlayheadMapper> m_mapper = std::make_shared<PlayheadMapper>();
    std::atomic<int64_t> m_endPosition { 0 };
    std::optional<RealtimeEffects::InitializationScope> m_effectsInitialization;
    //! Of the device callback
    std::unique_ptr<BlockContext> m_blockContext;
    //! Of the tracks whose workers render their whole chains
    std::vector<std::unique_ptr<BlockContext> > m_trackContexts;
    std::vector<Track> m_tracks;
    //! How far ahead of what it renders the master's chain reads the sequences, at most
    int64_t m_maxLatency = 0;
    std::shared_ptr<MeterTapNode> m_masterTap;
    audio::IAudioMeterPtr m_meter;
    //! If there is a meter: what the chains render for it, which the device callback forwards as it plays
    std::unique_ptr<MeterSink> m_meterSink;
    //! What plays: the master's chain
    std::shared_ptr<ISequenceProcessingNode> m_top;

    std::atomic<bool> m_paused { false };
    std::atomic<bool> m_finished { false };

    // Device thread
    std::vector<float> m_buffer[numChannels];
    float* m_bufferPointers[numChannels] {};
};
}
