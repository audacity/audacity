/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "audio/iaudioengine.h"

namespace au::au3audio::tests {
class AudioEngineMock : public au::audio::IAudioEngine
{
public:
    MOCK_METHOD(bool, isBusy, (), (const, override));

    MOCK_METHOD(int, startStream, (const TransportSequences& sequences, double startTime, double endTime, double mixerEndTime,
                                   AudacityProject & project, bool isDefaultPlayTrackPolicy, double audioStreamSampleRate,
                                   std::optional<double> startTimeOverride), (override));
    MOCK_METHOD(void, stopStream, (), (override));
    MOCK_METHOD(void, pauseStream, (bool pause), (override));
    MOCK_METHOD(void, seekStream, (double time), (override));

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
    MOCK_METHOD(std::optional<au::audio::AudioCallbackInfo>, consumeNextCallbackInfo, (), (override));

    MOCK_METHOD(muse::async::Notification, updateRequested, (), (const, override));
    MOCK_METHOD(muse::async::Notification, commitRequested, (), (const, override));
    MOCK_METHOD(muse::async::Notification, finished, (), (const, override));
    MOCK_METHOD((muse::async::Channel<au3::Au3TrackId, au3::Au3ClipId>), recordingClipChanged, (), (const, override));
};
}
