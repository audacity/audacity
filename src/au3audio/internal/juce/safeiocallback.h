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

#include "juceincludes.h"
#include "playedrange.h"
#include "playheadmapper.h"
#include "stereo.h"
#include "track.h"

class AudacityProject;
class LockFreeQueueNode;

namespace au::au4juce {
class BlockContext;
class MeterSink;
class RecordingCapture;
class MeterTapNode;

//! Plays safe: everything renders ahead of the device on a worker, behind one queue, as AudioIO
/*!
 * Peaks of processing are absorbed by the queue; what isn't rendered in time plays late, after silence. Changes are heard
 * once what was rendered before them has played. The device callback only reads the queue, applies the playback volume,
 * and reports what plays.
 */
class SafeIoCallback final : public juce::AudioIODeviceCallback
{
public:
    //! Reports what it plays to `callbackInfos` and `playedRanges`, and applies `playbackVolume`
    SafeIoCallback(AudacityProject& project, int token, std::unique_ptr<juce::AudioIODevice> device, bool followsPlayRegion,
                   LockFreeQueue<audio::AudioCallbackInfo>& callbackInfos, LockFreeQueue<PlayedRange>& playedRanges,
                   const std::atomic<float>& playbackVolume);
    ~SafeIoCallback() override;

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
    void stop();

    RealtimeEffects::InitializationScope* effectsInitialization();

    // juce::AudioIODeviceCallback
    void audioDeviceIOCallbackWithContext(const float* const* inputChannelData, int numInputChannels, float* const* outputChannelData,
                                          int numOutputChannels, int numSamples, const juce::AudioIODeviceCallbackContext&) override;
    void audioDeviceAboutToStart(juce::AudioIODevice*) override {}
    void audioDeviceStopped() override {}
    void audioDeviceError(const juce::String& errorMessage) override;

    AudacityProject& project;
    const int token;
    //! Whether loop play follows the play region, as with the default playback policy
    const bool followsPlayRegion;
    double trackEndTime = 0.0;

private:
    //! What is rendered ahead is rendered in chunks of this duration ...
    static constexpr double readAheadChunkDuration = 0.05;
    //! ... up to this many ahead: as AudioIO, which buffers 0.25 s in batches of 0.05 s
    static constexpr size_t readAheadNumChunks = 5;

    int64_t toSamples(double time) const { return static_cast<int64_t>(std::llround(time * m_sampleRate)); }
    double toTime(int64_t samples) const { return samples / m_sampleRate; }
    size_t readAheadChunkSize() const { return static_cast<size_t>(std::lround(readAheadChunkDuration * m_sampleRate)); }

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

    const std::weak_ptr<AudacityProject> m_wProject;
    const double m_sampleRate;
    const int m_blockSize;
    LockFreeQueue<audio::AudioCallbackInfo>& m_callbackInfos;
    LockFreeQueue<PlayedRange>& m_playedRanges;
    const std::atomic<float>& m_playbackVolume;

    std::unique_ptr<juce::AudioIODevice> m_device;
    std::unique_ptr<RecordingCapture> m_capture;
    const std::shared_ptr<PlayheadMapper> m_mapper = std::make_shared<PlayheadMapper>();
    std::atomic<int64_t> m_endPosition { 0 };
    std::optional<RealtimeEffects::InitializationScope> m_effectsInitialization;
    std::unique_ptr<BlockContext> m_blockContext;
    std::vector<Track> m_tracks;
    std::shared_ptr<MeterTapNode> m_masterTap;
    audio::IAudioMeterPtr m_meter;
    //! If there is a meter: what the queue's worker renders for it, which the device callback forwards as it plays
    std::unique_ptr<MeterSink> m_meterSink;
    std::unique_ptr<LockFreeQueueNode> m_queue;
    //! How far ahead of what it renders the master's chain reads the sequences, at most
    int64_t m_maxLatency = 0;

    std::atomic<bool> m_paused { false };
    std::atomic<bool> m_finished { false };

    // Device thread
    std::vector<float> m_buffer[numChannels];
    float* m_bufferPointers[numChannels] {};
};
}
