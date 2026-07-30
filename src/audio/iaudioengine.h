/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <chrono>
#include <functional>
#include <optional>
#include <vector>

#include "framework/global/async/channel.h"
#include "framework/global/async/notification.h"
#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/types/string.h"

#include "au3wrap/au3types.h"
#include "audioconfigurationtypes.h"

struct TransportSequences;
struct AudioIOStartStreamOptions;
namespace au::audio {
struct AudioCallbackInfo {
    std::chrono::steady_clock::time_point dacTime;
    int numSamples = 0;
};

class IAudioEngine : MODULE_GLOBAL_INTERFACE
{
    INTERFACE_ID(IAudioEngine);
public:
    virtual ~IAudioEngine() = default;

    virtual bool isBusy() const = 0;
    virtual bool isCapturing() const = 0;
    virtual bool isMonitoring() const = 0;
    // Includes transitional busy/inactive stream states.
    virtual std::optional<AudioStreamDescriptor> currentStream() const = 0;

    struct StartStreamOptions {
        bool isDefaultPolicy = true;
        double sampleRate = 0.0;
        double leadInTime = 0.0;
        std::vector<std::vector<float> >* crossfadeData = nullptr;
        //! Does not change the play region.
        std::optional<double> streamStartTime;
        //! Open the input device speculatively so that capture can be armed
        //! later without restarting the stream (deferred capture).
        bool openCaptureChannels = false;
    };

    virtual int startStream(const TransportSequences& sequences, double startTime, double endTime, double mixerEndTime, // Time at which mixer stops producing, maybe > endTime
                            AudacityProject& project, const StartStreamOptions& options) = 0;
    virtual void stopStream() = 0;
    virtual void pauseStream(bool pause) = 0;
    virtual void seekStream(double time) = 0;

    virtual void startMonitoring(AudacityProject& project) = 0;
    virtual void stopMonitoring() = 0;

    //! Deferred capture: arm/disarm recording on a running playback stream that
    //! opened its capture channels speculatively (openCaptureChannels)
    virtual bool canArmCapture() const = 0;
    virtual bool isCaptureArmed() const = 0;
    //! @return the track time of the punch point on success. If given, onArm is
    //! invoked with the punch time before any input flows — a race-free window
    //! to position the recording clips.
    virtual std::optional<double> armCapture(const TransportSequences& sequences,
                                             const std::function<void(double punchTime)>& onArm = {}) = 0;
    //! Commits the captured audio and keeps the stream running
    virtual bool disarmCapture() = 0;
    //! The recording was committed while the stream keeps running
    virtual muse::async::Notification captureStopped() const = 0;

    virtual void setInputVolume(float newInputVolume) = 0;
    virtual float getInputVolume() const = 0;
    virtual void setPlaybackVolume(float newPlaybackVolume) = 0;
    virtual float getPlaybackVolume() const = 0;

    virtual bool canStopAudioStream(AudacityProject& project) const = 0;

    virtual void handleDeviceChange() = 0;

    virtual muse::String lastErrorString() const = 0;
    virtual double getPlaybackSampleRate() const = 0;
    virtual void updateTimePosition(unsigned long newlyConsumedSamples) = 0;
    virtual std::optional<AudioCallbackInfo> consumeNextCallbackInfo() = 0;

    virtual muse::async::Notification updateRequested() const = 0;
    virtual muse::async::Notification commitRequested() const = 0;
    virtual muse::async::Notification finished() const = 0;
    virtual muse::async::Channel<au3::Au3TrackId, au3::Au3ClipId> recordingClipChanged() const = 0;
};
}
