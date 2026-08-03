/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "audio/driver/iaudiodrivercontroller.h"

namespace au::audio {
class AudioDriverControllerMock : public IAudioDriverController
{
public:
    MOCK_METHOD(AudioConfiguration, configuration, (), (const, override));
    MOCK_METHOD(muse::async::Channel<AudioConfigurationDelta>, configurationChanged, (), (const, override));

    MOCK_METHOD(std::vector<std::string>, apis, (), (const, override));
    MOCK_METHOD(std::vector<std::string>, outputDevices, (), (const, override));
    MOCK_METHOD(std::vector<std::string>, outputDevices, (const std::string&), (const, override));
    MOCK_METHOD(std::string, systemDefaultOutputDevice, (const std::string&), (const, override));
    MOCK_METHOD(std::vector<std::string>, inputDevices, (), (const, override));
    MOCK_METHOD(std::vector<std::string>, inputDevices, (const std::string&), (const, override));
    MOCK_METHOD(std::string, systemDefaultInputDevice, (const std::string&), (const, override));
    MOCK_METHOD(int, inputChannelsAvailable, (), (const, override));
    MOCK_METHOD(int, inputChannelsAvailable, (const std::string&, const AudioDeviceSelection&), (const, override));
    MOCK_METHOD(std::vector<uint64_t>, sampleRates, (), (const, override));
    MOCK_METHOD(std::vector<std::string>, sampleFormats, (), (const, override));

    MOCK_METHOD(ApplyResult, apply,
                (const muse::modularity::ContextPtr&, const AudioConfigurationChange&), (override));
    MOCK_METHOD(ApplyResult, rescan, (), (override));
    MOCK_METHOD(ApplyResult, reload, (const muse::modularity::ContextPtr&), (override));
    MOCK_METHOD(ApplyResult, openAsioDriverSettings,
                (const AudioRoutingChange&), (override));

    MOCK_METHOD(muse::async::Notification, audioDeviceListChanged, (), (const, override));
    MOCK_METHOD(muse::async::Channel<std::string>, usedOutputDeviceChanged, (), (const, override));
    MOCK_METHOD(muse::async::Channel<std::string>, usedInputDeviceChanged, (), (const, override));
};
}
