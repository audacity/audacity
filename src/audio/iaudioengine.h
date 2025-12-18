/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <chrono>
#include <optional>

#include "framework/global/async/channel.h"
#include "framework/global/async/notification.h"
#include "framework/global/modularity/imoduleinterface.h"
#include "framework/global/types/string.h"

#include "au3wrap/au3types.h"

struct TransportSequences;
struct AudioIOStartStreamOptions;
namespace au::audio {
struct AudioCallbackInfo {
    std::chrono::steady_clock::time_point dacTime;
    int numSamples = 0;
};

class IAudioEngine : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAudioEngine);
public:
    virtual ~IAudioEngine() = default;

    virtual bool isBusy() const = 0;

    virtual int startStream(const TransportSequences& sequences, double startTime, double endTime, double mixerEndTime, // Time at which mixer stops producing, maybe > endTime
                            AudacityProject& project, bool isDefaultPlayTrackPolicy, double audioStreamSampleRate) = 0;
    virtual void stopStream() = 0;
    virtual void pauseStream(bool pause) = 0;
    virtual void seekStream(double time) = 0;

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

    virtual muse::async::Notification updateRequested() const = 0;
    virtual muse::async::Notification commitRequested() const = 0;
    virtual muse::async::Notification finished() const = 0;
    virtual muse::async::Channel<au3::Au3TrackId, au3::Au3ClipId> recordingClipChanged() const = 0;
};
}
