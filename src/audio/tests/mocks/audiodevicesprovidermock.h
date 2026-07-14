/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "audio/iaudiodevicesprovider.h"

namespace au::audio {
class AudioDevicesProviderMock : public IAudioDevicesProvider
{
public:
    MOCK_METHOD(std::vector<std::string>, outputDevices, (), (const, override));
    MOCK_METHOD(std::optional<std::string>, currentOutputDevice, (), (const, override));
    MOCK_METHOD(void, setOutputDevice, (const std::optional<std::string>&), (override));
    MOCK_METHOD(muse::async::Notification, outputDeviceChanged, (), (const, override));
    MOCK_METHOD(std::string, systemDefaultOutputDevice, (), (const, override));

    MOCK_METHOD(std::vector<std::string>, inputDevices, (), (const, override));
    MOCK_METHOD(std::optional<std::string>, currentInputDevice, (), (const, override));
    MOCK_METHOD(void, setInputDevice, (const std::optional<std::string>&), (override));
    MOCK_METHOD(muse::async::Notification, inputDeviceChanged, (), (const, override));
    MOCK_METHOD(std::string, systemDefaultInputDevice, (), (const, override));
    MOCK_METHOD(bool, hasRecordingDevices, (), (const, override));

    MOCK_METHOD(std::vector<std::string>, apis, (), (const, override));
    MOCK_METHOD(std::string, currentApi, (), (const, override));
    MOCK_METHOD(void, setApi, (const std::string&), (override));
    MOCK_METHOD(muse::async::Notification, apiChanged, (), (const, override));

    MOCK_METHOD(int, inputChannelsAvailable, (), (const, override));
    MOCK_METHOD(int, inputChannelsSelected, (), (const, override));
    MOCK_METHOD(void, setInputChannels, (const int), (override));
    MOCK_METHOD(muse::async::Notification, inputChannelsAvailableChanged, (), (const, override));
    MOCK_METHOD(muse::async::Notification, inputChannelsChanged, (), (const, override));

    MOCK_METHOD(double, bufferLength, (), (const, override));
    MOCK_METHOD(void, setBufferLength, (double), (override));
    MOCK_METHOD(muse::async::Notification, bufferLengthChanged, (), (const, override));

    MOCK_METHOD(bool, automaticCompensationEnabled, (), (const, override));
    MOCK_METHOD(void, setAutomaticCompensationEnabled, (bool), (override));
    MOCK_METHOD(muse::async::Notification, automaticCompensationEnabledChanged, (), (const, override));

    MOCK_METHOD(double, latencyCompensation, (), (const, override));
    MOCK_METHOD(void, setLatencyCompensation, (double), (override));
    MOCK_METHOD(muse::async::Notification, latencyCompensationChanged, (), (const, override));

    MOCK_METHOD(std::vector<uint64_t>, sampleRates, (), (const, override));
    MOCK_METHOD(uint64_t, defaultSampleRate, (), (const, override));
    MOCK_METHOD(void, setDefaultSampleRate, (uint64_t), (override));
    MOCK_METHOD(muse::async::Notification, defaultSampleRateChanged, (), (const, override));

    MOCK_METHOD(std::vector<std::string>, sampleFormats, (), (const, override));
    MOCK_METHOD(std::string, defaultSampleFormat, (), (const, override));
    MOCK_METHOD(void, setDefaultSampleFormat, (const std::string&), (override));
    MOCK_METHOD(muse::async::Notification, defaultSampleFormatChanged, (), (const, override));

    MOCK_METHOD(bool, asioUseDeviceSampleRate, (), (const, override));
    MOCK_METHOD(void, setAsioUseDeviceSampleRate, (bool), (override));
    MOCK_METHOD(muse::async::Notification, asioUseDeviceSampleRateChanged, (), (const, override));

    MOCK_METHOD(void, showAsioControlPanel, (), (override));
    MOCK_METHOD(void, handleDeviceChange, (), (override));
    MOCK_METHOD(void, rescan, (), (override));
};
}
