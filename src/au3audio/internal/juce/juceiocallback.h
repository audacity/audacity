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
class ISequenceProcessingNode;

namespace au::au4juce {
class BlockContext;
class MeterSink;
class MonitoredInput;
class RecordingCapture;
class MeterTapNode;

//! A stream on a JUCE device: what it plays, and what it records if it does
/*!
 * It builds the tracks' chains and the master's, plays them as the playhead says, records, applies the playback volume,
 * and reports what plays and what the meters show. Where the chains render, and so what is read ahead and by which
 * thread, is the subclasses': see SafeIoCallback and FastIoCallback.
 */
class JuceIoCallback : public juce::AudioIODeviceCallback
{
public:
    //! Reports what it plays to `callbackInfos` and `playedRanges`, and applies `playbackVolume`
    JuceIoCallback(AudacityProject& project, int token, std::unique_ptr<juce::AudioIODevice> device, bool followsPlayRegion,
                   LockFreeQueue<audio::AudioCallbackInfo>& callbackInfos, LockFreeQueue<PlayedRange>& playedRanges,
                   const std::atomic<float>& playbackVolume);
    //! Subclasses stop() in their destructor, as it releases what they render with
    ~JuceIoCallback() override;

    double sampleRate() const { return m_sampleRate; }

    //! Where a recording starts and ends, in the timeline, in seconds
    struct RecordingRange {
        double start = 0.0;
        double end = 0.0;
    };

    //! The tracks and the master's chain, for `sequences`, sending to `meter`, if any, what they play. If recording, as
    //! AudioIO, the tracks stop playing where the recording ends, and those recorded into with a lead-in where it starts.
    void build(const TransportSequences& sequences, const audio::IAudioMeterPtr& meter, std::optional<RecordingRange> recording);
    //! Monitors the input through `track`'s effects, or only the master's if null; call before build(). For subclasses
    //! that render in the device callback.
    void monitorThrough(std::shared_ptr<const PlayableSequence> track, int numInputChannels, const std::atomic<float>& inputVolume);
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

    // juce::AudioIODeviceCallback
    void audioDeviceIOCallbackWithContext(const float* const* inputChannelData, int numInputChannels, float* const* outputChannelData,
                                          int numOutputChannels, int numSamples, const juce::AudioIODeviceCallbackContext&) final;
    void audioDeviceAboutToStart(juce::AudioIODevice*) final {}
    void audioDeviceStopped() final {}
    void audioDeviceError(const juce::String& errorMessage) final;

    AudacityProject& project;
    const int token;
    //! Whether loop play follows the play region, as with the default playback policy
    const bool followsPlayRegion;
    double trackEndTime = 0.0;

protected:
    //! What is read ahead is read in chunks of this duration ...
    static constexpr double readAheadChunkDuration = 0.05;
    //! ... up to this many ahead: as AudioIO, which buffers 0.25 s in batches of 0.05 s
    static constexpr size_t readAheadNumChunks = 5;
    //! A worker that can't keep up, or after a seek, resumes at most this many chunks ahead of what reads it
    static constexpr size_t maxResyncLeadNumChunks = 2;

    int64_t toSamples(double time) const { return static_cast<int64_t>(std::llround(time * m_sampleRate)); }
    double toTime(int64_t samples) const { return samples / m_sampleRate; }
    size_t readAheadChunkSize() const { return static_cast<size_t>(std::lround(readAheadChunkDuration * m_sampleRate)); }

    //! In logs
    virtual const char* name() const = 0;
    //! How the tracks read their sequences ahead, if they do
    virtual std::optional<TrackReadAhead> trackReadAhead() const = 0;
    //! What sums `tracks`, their chains' outputs, rendering at most `maxFrames` at once: one after the other, by default
    virtual std::shared_ptr<ISequenceProcessingNode> makeMix(std::vector<std::shared_ptr<ISequenceProcessingNode> > tracks,
                                                             size_t maxFrames);
    //! Once built: `top` is the master's chain, which renders what plays
    virtual void onBuilt(std::shared_ptr<ISequenceProcessingNode> top) = 0;
    //! Once the playhead is set
    virtual void startReadingAhead() = 0;
    //! Any thread, after the playhead changed what it plays ahead of the device: what was read ahead isn't played
    virtual void invalidateReadAhead() = 0;
    //! Any thread: the stream index that what was read ahead, or is being read, ends at, as the sequences are read
    virtual int64_t readAheadEnd() const = 0;
    //! Device thread: renders the `frames` frames of the stream from `index` into `stereo`. Returns how many, from the
    //! first, it rendered: the stream advances by as many.
    virtual size_t render(float* const* stereo, int64_t index, size_t frames) = 0;
    //! Before the chains are released, once the device stopped: what renders them
    virtual void releaseRendering() {}
    //! More of the dropouts, for the log, if any
    virtual std::string dropoutDetails() const { return {}; }

    std::vector<Track> m_tracks;
    //! How far ahead of what it renders the master's chain reads the sequences, at most
    int64_t m_maxLatency = 0;
    const double m_sampleRate;

private:
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
    const int m_blockSize;
    LockFreeQueue<audio::AudioCallbackInfo>& m_callbackInfos;
    LockFreeQueue<PlayedRange>& m_playedRanges;
    const std::atomic<float>& m_playbackVolume;

    std::unique_ptr<juce::AudioIODevice> m_device;
    std::unique_ptr<RecordingCapture> m_capture;

    // If the input is monitored through the effects: what monitorThrough() said, then what the chains read of it
    struct MonitorSettings {
        std::shared_ptr<const PlayableSequence> track;
        int numInputChannels = 0;
        const std::atomic<float>* inputVolume = nullptr;
    };
    std::optional<MonitorSettings> m_monitorSettings;
    std::shared_ptr<MonitoredInput> m_monitoredInput;
    const std::shared_ptr<PlayheadMapper> m_mapper = std::make_shared<PlayheadMapper>();
    std::atomic<int64_t> m_endPosition { 0 };
    std::optional<RealtimeEffects::InitializationScope> m_effectsInitialization;
    std::unique_ptr<BlockContext> m_blockContext;
    std::shared_ptr<MeterTapNode> m_masterTap;
    audio::IAudioMeterPtr m_meter;
    //! If there is a meter: what the chains render for it, which the device callback forwards as it plays
    std::unique_ptr<MeterSink> m_meterSink;
    bool m_built = false;

    std::atomic<bool> m_paused { false };
    std::atomic<bool> m_finished { false };

    //! Playback dropouts, to compare the audio engines: written by the callback, read once the device stopped
    struct Dropouts {
        //! What the callback output while playing
        int64_t frames = 0;
        //! Callbacks that took longer than the audio they output
        int lateCallbacks = 0;
        double longestCallback = 0.0;
        //! The block size of the longest callback
        double longestCallbackBlock = 0.0;
    };
    Dropouts m_dropouts;

    // Device thread
    std::vector<float> m_buffer[numChannels];
    float* m_bufferPointers[numChannels] {};
};
}
