/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "audio/iaudioengine.h"

// gmock needs the complete TrackId type used by recordingClipChanged().
#include "au3-track/Track.h"

namespace au::audio {
class AudioEngineMock : public IAudioEngine
{
public:
    MOCK_METHOD(bool, isBusy, (), (const, override));
    MOCK_METHOD(bool, isCapturing, (), (const, override));
    MOCK_METHOD(bool, isMonitoring, (), (const, override));
    MOCK_METHOD(std::optional<AudioStreamDescriptor>, currentStream, (), (const, override));
    MOCK_METHOD(bool, isStreamActive, (AudacityProject &), (const, override));
    MOCK_METHOD(double, streamTime, (), (const, override));

    MOCK_METHOD(int, startStream, (const TransportSequences& sequences, double startTime, double endTime, double mixerEndTime,
                                   AudacityProject & project, const StartStreamOptions& options), (override));
    MOCK_METHOD(void, stopStream, (), (override));
    MOCK_METHOD(void, pauseStream, (bool pause), (override));
    MOCK_METHOD(void, seekStream, (double time), (override));

    MOCK_METHOD(std::shared_ptr<RealtimeEffectState>, addRealtimeEffectState,
                (AudacityProject&, ChannelGroup*, const std::string&), (override));
    MOCK_METHOD(void, removeRealtimeEffectState,
                (AudacityProject&, ChannelGroup*, const std::shared_ptr<RealtimeEffectState>&), (override));
    MOCK_METHOD(std::shared_ptr<RealtimeEffectState>, replaceRealtimeEffectState,
                (AudacityProject&, ChannelGroup*, size_t, const std::string&), (override));

    MOCK_METHOD(void, startMonitoring, (AudacityProject & project), (override));
    MOCK_METHOD(void, stopMonitoring, (), (override));
    MOCK_METHOD(void, setInputVolume, (float newInputVolume), (override));
    MOCK_METHOD(float, getInputVolume, (), (const, override));
    MOCK_METHOD(void, setPlaybackVolume, (float newPlaybackVolume), (override));
    MOCK_METHOD(float, getPlaybackVolume, (), (const, override));

    MOCK_METHOD(bool, canStopAudioStream, (AudacityProject & project), (const, override));

    MOCK_METHOD(void, handleDeviceChange, (), (override));

    MOCK_METHOD(muse::String, lastErrorString, (), (const, override));
    MOCK_METHOD(double, getPlaybackSampleRate, (), (const, override));
    MOCK_METHOD(void, updateTimePosition, (unsigned long newlyConsumedSamples), (override));
    MOCK_METHOD(std::optional<AudioCallbackInfo>, consumeNextCallbackInfo, (), (override));

    MOCK_METHOD(muse::async::Notification, recordingUpdateRequested, (), (const, override));
    MOCK_METHOD(muse::async::Notification, recordingCommitRequested, (), (const, override));
    MOCK_METHOD(muse::async::Notification, recordingFinished, (), (const, override));
    MOCK_METHOD(muse::async::Notification, streamStopped, (), (const, override));
    MOCK_METHOD((muse::async::Channel<au3::Au3TrackId, au3::Au3ClipId>), recordingClipChanged, (), (const, override));
};
}
