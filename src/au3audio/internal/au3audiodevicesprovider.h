/*
* Audacity: A Digital Audio Editor
*/

#ifndef AU_AU3WRAP_AU3AUDIODEVICESPROVIDER_H
#define AU_AU3WRAP_AU3AUDIODEVICESPROVIDER_H

#include <chrono>

#include "framework/global/modularity/ioc.h"
#include "global/async/asyncable.h"
#include "global/timer.h"

#include "muse_framework_config.h"

#include "context/iglobalcontext.h"
#include "audio/iaudiodevicesprovider.h"
#include "audio/iaudioengine.h"

#ifdef MUSE_ENABLE_UNIT_TESTS
#include <gtest/gtest_prod.h>
#endif

namespace au::au3audio {
class IAu3DeviceManager;
}

namespace au::au3audio {
class Au3AudioDevicesProvider : public audio::IAudioDevicesProvider, public muse::async::Asyncable, public muse::Injectable
{
    muse::Inject<context::IGlobalContext> globalContext { this };
    muse::Inject<au::audio::IAudioEngine> audioEngine { this };

public:
    Au3AudioDevicesProvider(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx),
        m_defaultDevicePollTimer(std::chrono::milliseconds(1000))
    {}

    void init();

    std::vector<std::string> outputDevices() const override;
    std::string currentOutputDevice() const override;
    void setOutputDevice(const std::string& device) override;
    muse::async::Notification outputDeviceChanged() const override;

    std::vector<std::string> inputDevices() const override;
    std::string currentInputDevice() const override;
    void setInputDevice(const std::string& device) override;
    muse::async::Notification inputDeviceChanged() const override;

    void handleDeviceChange() override;

    std::vector<std::string> apis() const override;
    std::string currentApi() const override;
    void setApi(const std::string& api) override;
    muse::async::Notification apiChanged() const override;

    int inputChannelsAvailable() const override;
    int inputChannelsSelected() const override;
    void setInputChannels(const int count) override;
    muse::async::Notification inputChannelsAvailableChanged() const override;
    muse::async::Notification inputChannelsChanged() const override;

    double bufferLength() const override;
    void setBufferLength(double newBufferLength) override;
    muse::async::Notification bufferLengthChanged() const override;

    double latencyCompensation() const override;
    void setLatencyCompensation(double newLatencyCompensation) override;
    muse::async::Notification latencyCompensationChanged() const override;

    std::vector<uint64_t> sampleRates() const override;
    uint64_t defaultSampleRate() const override;
    void setDefaultSampleRate(uint64_t newRate) override;
    muse::async::Notification defaultSampleRateChanged() const override;

    std::vector<std::string> sampleFormats() const override;
    std::string defaultSampleFormat() const override;
    void setDefaultSampleFormat(const std::string& format) override;
    muse::async::Notification defaultSampleFormatChanged() const override;

    void rescan() override;

#ifdef MUSE_ENABLE_UNIT_TESTS
    void setDeviceManagerForTests(IAu3DeviceManager* deviceManager);
#endif

private:
#ifdef MUSE_ENABLE_UNIT_TESTS
    FRIEND_TEST(Au3AudioDevicesProviderTests, OutputDevicesIncludeSystemDefaultAndCurrentSelectionIsLabelWhenUnset);
    FRIEND_TEST(Au3AudioDevicesProviderTests, InputDevicesIncludeSystemDefaultAndCurrentSelectionIsLabelWhenUnset);
    FRIEND_TEST(Au3AudioDevicesProviderTests, SetOutputDeviceSystemDefaultClearsSettingsAndWrap);
    FRIEND_TEST(Au3AudioDevicesProviderTests, SetInputDeviceSystemDefaultClearsSettingsAndWrap);
#endif

    void initHosts();
    void initInputChannels();

    void updateInputOutputDevices();
    void setupInputDevice(const std::string& newDevice);

    std::string defaultOutputDevice();
    std::string defaultInputDevice();
    std::string systemDefaultOutputDevice() const;
    std::string systemDefaultInputDevice() const;
    void checkSystemDefaultDeviceChanges();
    IAu3DeviceManager& deviceManager() const;

    std::vector<std::string> m_audioApis;
    std::vector<std::string> m_outputDevices;
    std::vector<std::string> m_inputDevices;
    int m_inputChannelsAvailable = 0;

    muse::async::Notification m_audioOutputDeviceChanged;
    muse::async::Notification m_audioInputDeviceChanged;
    muse::async::Notification m_audioApiChanged;
    muse::async::Notification m_inputChannelsChanged;
    muse::async::Notification m_inputChannelsListChanged;
    muse::async::Notification m_bufferLengthChanged;
    muse::async::Notification m_latencyCompensationChanged;
    muse::async::Notification m_defaultSampleRateChanged;
    muse::async::Notification m_defaultSampleFormatChanged;

    muse::Timer m_defaultDevicePollTimer;
    std::string m_lastDefaultOutputDevice;
    std::string m_lastDefaultInputDevice;
    bool m_pendingDeviceChange = false;
    IAu3DeviceManager* m_deviceManager = nullptr;
};
}

#endif // AU_AU3WRAP_AU3AUDIODEVICESPROVIDER_H
