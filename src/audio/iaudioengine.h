/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <chrono>
#include <memory>
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
class ChannelGroup;
class RealtimeEffectState;
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
    //! Whether the audio stream started by the given project is still running.
    //! Reports false as soon as the stream has played out its last buffer (or was stopped),
    //! even before stopStream() has been called — whereas currentStream() also covers the
    //! busy window that lasts until stopStream() releases the stream.
    virtual bool isStreamActive(AudacityProject& project) const = 0;
    //! Playback time of the active stream.
    virtual double streamTime() const = 0;

    struct StartStreamOptions {
        bool isDefaultPolicy = true;
        double sampleRate = 0.0;
        double leadInTime = 0.0;
        std::vector<std::vector<float> >* crossfadeData = nullptr;
        //! Does not change the play region.
        std::optional<double> streamStartTime;
    };

    //! Returns a positive stream token on success, 0 otherwise.
    //! On success the token is recorded as the project's audio IO token.
    virtual int startStream(const TransportSequences& sequences, double startTime, double endTime, double mixerEndTime, // Time at which mixer stops producing, maybe > endTime
                            AudacityProject& project, const StartStreamOptions& options) = 0;
    virtual void stopStream() = 0;
    virtual void pauseStream(bool pause) = 0;
    virtual void seekStream(double time) = 0;

    // Realtime effect stack of the live stream
    virtual std::shared_ptr<RealtimeEffectState> addRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                                                        const std::string& effectId) = 0;
    virtual void removeRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                           const std::shared_ptr<RealtimeEffectState>& state) = 0;
    virtual std::shared_ptr<RealtimeEffectState> replaceRealtimeEffectState(AudacityProject& project, ChannelGroup* group,
                                                                            size_t effectListIndex, const std::string& newEffectId) = 0;

    virtual void startMonitoring(AudacityProject& project) = 0;
    virtual void stopMonitoring() = 0;

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

    virtual muse::async::Notification recordingUpdateRequested() const = 0;
    virtual muse::async::Notification recordingCommitRequested() const = 0;
    virtual muse::async::Notification recordingFinished() const = 0;
    virtual muse::async::Notification streamStopped() const = 0;
    virtual muse::async::Channel<au3::Au3TrackId, au3::Au3ClipId> recordingClipChanged() const = 0;
};
}
