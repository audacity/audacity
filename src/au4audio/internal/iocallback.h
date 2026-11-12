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

//! A stream on a device: what it plays
/*!
 * It builds the tracks' chains and the master's, plays them as the playhead says, applies the playback volume, and
 * reports what plays.
 *
 * For now, the tracks' chains and the master's render in the device callback.
 */
class IoCallback final : public audio::IDeviceCallback
{
public:
    //! Reports what it plays to `callbackInfos` and `playedRanges`, and applies `playbackVolume`
    IoCallback(AudacityProject& project, int token, std::unique_ptr<audio::IDevice> device, bool followsPlayRegion,
               LockFreeQueue<audio::AudioCallbackInfo>& callbackInfos, LockFreeQueue<PlayedRange>& playedRanges,
               const std::atomic<float>& playbackVolume);
    ~IoCallback() override;

    double sampleRate() const { return m_sampleRate; }

    //! The tracks and the master's chain, for `sequences`
    void build(const TransportSequences& sequences);
    //! Plays from `startTime` to `endTime`, or loops `loopRange` if given, rolling into it if `startTime` is left of it
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
    int64_t toSamples(double time) const { return static_cast<int64_t>(std::llround(time * m_sampleRate)); }
    double toTime(int64_t samples) const { return samples / m_sampleRate; }

    //! Device thread: renders the `frames` frames of the stream from `index` into `stereo`. Returns how many, from the
    //! first, it rendered: the stream advances by as many.
    size_t render(float* const* stereo, int64_t index, size_t frames);
    //! Device thread: plays `frames` frames from `offset` on, at most the block size. Returns how many, at the end, it
    //! couldn't play, being silent instead: the transport doesn't advance by them.
    int renderChunk(float* const* output, int numOutputChannels, int offset, int frames);
    //! `stereo` goes out with the playback volume; a mono device gets the downmix
    void writeOutput(const float* const* stereo, float* const* output, int numOutputChannels, int offset, int frames) const;
    void reportPlayed(int64_t start, int64_t frames);

    //! At least a sample long
    PlayheadMapper::Range toLoopRange(const std::pair<double, double>& loopRange) const;

    const double m_sampleRate;
    const std::weak_ptr<AudacityProject> m_wProject;
    const int m_blockSize;
    LockFreeQueue<audio::AudioCallbackInfo>& m_callbackInfos;
    LockFreeQueue<PlayedRange>& m_playedRanges;
    const std::atomic<float>& m_playbackVolume;

    std::unique_ptr<audio::IDevice> m_device;

    const std::shared_ptr<PlayheadMapper> m_mapper = std::make_shared<PlayheadMapper>();
    std::atomic<int64_t> m_endPosition { 0 };
    std::optional<RealtimeEffects::InitializationScope> m_effectsInitialization;
    std::unique_ptr<BlockContext> m_blockContext;
    std::vector<Track> m_tracks;
    //! What plays: the master's chain
    std::shared_ptr<ISequenceProcessingNode> m_top;

    std::atomic<bool> m_paused { false };
    std::atomic<bool> m_finished { false };

    // Device thread
    std::vector<float> m_buffer[numChannels];
    float* m_bufferPointers[numChannels] {};
};
}
