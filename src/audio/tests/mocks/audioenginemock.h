/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "audio/iaudioengine.h"

namespace au::audio {
class AudioEngineMock : public IAudioEngine
{
public:
    MOCK_METHOD(bool, isBusy, (), (const, override));
    MOCK_METHOD(bool, isCapturing, (), (const, override));

    MOCK_METHOD(int, startStream, (const TransportSequences&, double, double, double, AudacityProject&, bool, double, double,
                                   (std::vector<std::vector<float> >*)), (override));
    MOCK_METHOD(void, stopStream, (), (override));
    MOCK_METHOD(void, pauseStream, (bool), (override));
    MOCK_METHOD(void, seekStream, (double), (override));

    MOCK_METHOD(void, startMonitoring, (AudacityProject &), (override));
    MOCK_METHOD(void, stopMonitoring, (), (override));

    MOCK_METHOD(void, setInputVolume, (float), (override));
    MOCK_METHOD(float, getInputVolume, (), (const, override));
    MOCK_METHOD(void, setPlaybackVolume, (float), (override));
    MOCK_METHOD(float, getPlaybackVolume, (), (const, override));

    MOCK_METHOD(bool, canStopAudioStream, (AudacityProject &), (const, override));

    MOCK_METHOD(void, handleDeviceChange, (), (override));

    MOCK_METHOD(muse::String, lastErrorString, (), (const, override));
    MOCK_METHOD(double, getPlaybackSampleRate, (), (const, override));
    MOCK_METHOD(void, updateTimePosition, (unsigned long), (override));
    MOCK_METHOD(std::optional<AudioCallbackInfo>, consumeNextCallbackInfo, (), (override));

    MOCK_METHOD(muse::async::Notification, updateRequested, (), (const, override));
    MOCK_METHOD(muse::async::Notification, commitRequested, (), (const, override));
    MOCK_METHOD(muse::async::Notification, finished, (), (const, override));
    MOCK_METHOD((muse::async::Channel<au3::Au3TrackId, au3::Au3ClipId>), recordingClipChanged, (), (const, override));
};
}
