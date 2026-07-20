/*
* Audacity: A Digital Audio Editor
*/

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include "framework/global/settings.h"
#include "framework/global/modularity/ioc.h"

#include "audio/tests/mocks/audioenginemock.h"
#include "mocks/au3devicemanagerfake.h"

#include "au3audio/internal/au3audiodevicesprovider.h"
#include "au3audio/internal/platform/stub/stubsystemaudiodeviceslistener.h"

using ::testing::AtLeast;
using ::testing::NiceMock;

namespace au::au3audio {
namespace {
const muse::Settings::Key AUDIO_HOST("au3audio", "AudioIO/Host");
const muse::Settings::Key PLAYBACK_DEVICE("au3audio", "AudioIO/PlaybackDevice");
const muse::Settings::Key RECORDING_DEVICE("au3audio", "AudioIO/RecordingDevice");
const muse::Settings::Key INPUT_CHANNELS("au3audio", "AudioIO/RecordChannels");
const muse::Settings::Key RECORDING_SOURCE("au3audio", "AudioIO/RecordingSource");
}

class Au3AudioDevicesProviderTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_deviceManager = std::make_shared<Au3DeviceManagerFake>();
        muse::modularity::globalIoc()->registerExport<IAu3DeviceManager>("utests", m_deviceManager);

        m_audioEngine = std::make_shared<NiceMock<audio::AudioEngineMock> >();
        ON_CALL(*m_audioEngine, streamStopped()).WillByDefault(::testing::Return(m_engineStreamStopped));
        muse::modularity::globalIoc()->registerExport<audio::IAudioEngine>("utests", m_audioEngine);

        m_devicesListener = std::make_shared<StubSystemAudioDevicesListener>();
        muse::modularity::globalIoc()->registerExport<ISystemAudioDevicesListener>("utests", m_devicesListener);
    }

    void TearDown() override
    {
        m_provider.reset();
        muse::modularity::globalIoc()->unregister<IAu3DeviceManager>("utests");
        muse::modularity::globalIoc()->unregister<audio::IAudioEngine>("utests");
        muse::modularity::globalIoc()->unregister<ISystemAudioDevicesListener>("utests");
    }

    //! NOTE Settings are global state shared by all tests in the binary,
    //! so every test sets its full baseline before creating the provider
    void initProvider(const std::string& host, const std::string& recordingDevice = std::string(),
                      const std::string& playbackDevice = std::string(), int inputChannels = 1)
    {
        muse::settings()->setLocalValue(AUDIO_HOST, muse::Val(host));
        muse::settings()->setLocalValue(RECORDING_DEVICE, muse::Val(recordingDevice));
        muse::settings()->setLocalValue(PLAYBACK_DEVICE, muse::Val(playbackDevice));
        muse::settings()->setLocalValue(INPUT_CHANNELS, muse::Val(inputChannels));

        m_provider = std::make_shared<Au3AudioDevicesProvider>(std::make_shared<muse::modularity::Context>(100));
        m_provider->init();
    }

    std::shared_ptr<Au3DeviceManagerFake> m_deviceManager;
    std::shared_ptr<NiceMock<audio::AudioEngineMock> > m_audioEngine;
    std::shared_ptr<StubSystemAudioDevicesListener> m_devicesListener;
    muse::async::Notification m_engineStreamStopped;
    std::shared_ptr<Au3AudioDevicesProvider> m_provider;
};

TEST_F(Au3AudioDevicesProviderTests, SwitchApi_ToHostWithInputDevices_EnablesRecording)
{
    m_deviceManager->addOutputDevice("HostA", "Speakers A", 1);
    m_deviceManager->addInputDevice("HostB", "Mic B", 10, 2);
    m_deviceManager->setDefaultInputDevice("HostB", 10);

    initProvider("HostA");
    EXPECT_FALSE(m_provider->hasRecordingDevices());

    m_provider->setApi("HostB");

    EXPECT_TRUE(m_provider->hasRecordingDevices());
    EXPECT_EQ(m_provider->inputChannelsAvailable(), 2);
}

TEST_F(Au3AudioDevicesProviderTests, SwitchApi_ToHostWithoutInputDevices_DisablesRecording)
{
    m_deviceManager->addInputDevice("HostA", "Mic A", 1, 2);
    m_deviceManager->setDefaultInputDevice("HostA", 1);
    m_deviceManager->addOutputDevice("HostB", "Speakers B", 10);

    initProvider("HostA");
    EXPECT_TRUE(m_provider->hasRecordingDevices());

    m_provider->setApi("HostB");

    EXPECT_FALSE(m_provider->hasRecordingDevices());
    EXPECT_EQ(m_provider->inputChannelsAvailable(), 0);
}

TEST_F(Au3AudioDevicesProviderTests, SwitchApi_StoredDevicesMissing_ResetToSystemDefault)
{
    m_deviceManager->addInputDevice("HostA", "Mic A", 1, 2);
    m_deviceManager->addOutputDevice("HostA", "Speakers A", 2);
    m_deviceManager->addInputDevice("HostB", "Mic B", 10, 1);
    m_deviceManager->addOutputDevice("HostB", "Speakers B", 11);
    m_deviceManager->setDefaultInputDevice("HostB", 10);

    initProvider("HostA", "Mic A", "Speakers A");

    m_provider->setApi("HostB");

    EXPECT_EQ(m_provider->currentInputDevice(), std::nullopt);
    EXPECT_EQ(m_provider->currentOutputDevice(), std::nullopt);
    EXPECT_EQ(m_provider->inputChannelsAvailable(), 1);
}

TEST_F(Au3AudioDevicesProviderTests, SwitchApi_SameDeviceNameOnBothHosts_RefreshesChannelCount)
{
    m_deviceManager->addInputDevice("HostA", "USB Mic", 1, 2);
    m_deviceManager->addInputDevice("HostB", "USB Mic", 10, 1);

    initProvider("HostA", "USB Mic", std::string(), 2);
    EXPECT_EQ(m_provider->inputChannelsAvailable(), 2);
    EXPECT_EQ(m_provider->inputChannelsSelected(), 2);

    m_provider->setApi("HostB");

    EXPECT_EQ(m_provider->currentInputDevice(), std::optional<std::string>("USB Mic"));
    EXPECT_EQ(m_provider->inputChannelsAvailable(), 1);
    EXPECT_EQ(m_provider->inputChannelsSelected(), 1);
}

TEST_F(Au3AudioDevicesProviderTests, SwitchApi_SystemDefaultSelected_FollowsNewHostDefault)
{
    m_deviceManager->addInputDevice("HostA", "Mic A", 1, 1);
    m_deviceManager->setDefaultInputDevice("HostA", 1);
    m_deviceManager->addInputDevice("HostB", "Mic B", 10, 2);
    m_deviceManager->setDefaultInputDevice("HostB", 10);

    initProvider("HostA");
    EXPECT_EQ(m_provider->inputChannelsAvailable(), 1);

    m_provider->setApi("HostB");

    EXPECT_EQ(m_provider->currentInputDevice(), std::nullopt);
    EXPECT_EQ(m_provider->inputChannelsAvailable(), 2);
}

TEST_F(Au3AudioDevicesProviderTests, Rescan_SelectedDeviceUnplugged_FallsBackToSystemDefault)
{
    m_deviceManager->addInputDevice("HostA", "Mic A", 1, 2);
    m_deviceManager->addInputDevice("HostA", "Mic B", 2, 1);
    m_deviceManager->setDefaultInputDevice("HostA", 1);

    initProvider("HostA", "Mic B");
    EXPECT_EQ(m_provider->inputChannelsAvailable(), 1);

    EXPECT_CALL(*m_audioEngine, handleDeviceChange()).Times(AtLeast(1));

    m_deviceManager->inMaps.pop_back(); // Mic B unplugged
    m_provider->rescan();

    EXPECT_EQ(m_deviceManager->rescanCount, 1);
    EXPECT_EQ(m_provider->currentInputDevice(), std::nullopt);
    EXPECT_EQ(m_provider->inputChannelsAvailable(), 2);
}

TEST_F(Au3AudioDevicesProviderTests, Rescan_SystemDefaultChanged_FollowsNewDefault)
{
    m_deviceManager->addInputDevice("HostA", "Old Default", 1, 2);
    m_deviceManager->addInputDevice("HostA", "New Default", 2, 1);
    m_deviceManager->setDefaultInputDevice("HostA", 1);

    initProvider("HostA", std::string(), std::string(), 2);
    EXPECT_EQ(m_provider->inputChannelsAvailable(), 2);

    m_deviceManager->setDefaultInputDevice("HostA", 2);
    m_provider->rescan();

    EXPECT_EQ(m_provider->inputChannelsAvailable(), 1);
    EXPECT_EQ(m_provider->inputChannelsSelected(), 1);
}

TEST_F(Au3AudioDevicesProviderTests, Rescan_NoInputDevicesLeft_DisablesRecording)
{
    m_deviceManager->addInputDevice("HostA", "Mic A", 1, 2);
    m_deviceManager->addOutputDevice("HostA", "Speakers A", 2);
    m_deviceManager->setDefaultInputDevice("HostA", 1);

    initProvider("HostA");
    EXPECT_TRUE(m_provider->hasRecordingDevices());

    m_deviceManager->inMaps.clear();
    m_provider->rescan();

    EXPECT_FALSE(m_provider->hasRecordingDevices());
    EXPECT_EQ(m_provider->inputChannelsAvailable(), 0);
}

TEST_F(Au3AudioDevicesProviderTests, SetInputDevice_ClampsSelectedChannels)
{
    m_deviceManager->addInputDevice("HostA", "Stereo Mic", 1, 2);
    m_deviceManager->addInputDevice("HostA", "Mono Mic", 2, 1);
    m_deviceManager->setDefaultInputDevice("HostA", 1);

    initProvider("HostA", std::string(), std::string(), 2);
    EXPECT_EQ(m_provider->inputChannelsSelected(), 2);

    m_provider->setInputDevice("Mono Mic");

    EXPECT_EQ(m_provider->inputChannelsAvailable(), 1);
    EXPECT_EQ(m_provider->inputChannelsSelected(), 1);
}

TEST_F(Au3AudioDevicesProviderTests, SystemDevicesChanged_AutoRescans_AndReportsUsedInputDevice)
{
    m_deviceManager->addInputDevice("HostA", "Mic A", 1, 2);
    m_deviceManager->addInputDevice("HostA", "Mic B", 2, 1);
    m_deviceManager->setDefaultInputDevice("HostA", 1);

    initProvider("HostA");
    EXPECT_EQ(m_provider->inputChannelsAvailable(), 2);

    std::string reportedDevice;
    m_provider->usedInputDeviceChanged().onReceive(nullptr, [&reportedDevice](const std::string& device) {
        reportedDevice = device;
    });

    m_deviceManager->pendingSystemChange = [this]() {
        m_deviceManager->setDefaultInputDevice("HostA", 2);
    };
    m_devicesListener->systemDevicesChanged().notify();

    EXPECT_EQ(m_deviceManager->rescanCount, 1);
    EXPECT_EQ(reportedDevice, "Mic B");
    EXPECT_EQ(m_provider->inputChannelsAvailable(), 1);
}

TEST_F(Au3AudioDevicesProviderTests, SystemDevicesChanged_ReportsUsedOutputDevice)
{
    m_deviceManager->addOutputDevice("HostA", "Speakers", 1);
    m_deviceManager->addOutputDevice("HostA", "Headphones", 2);
    m_deviceManager->setDefaultOutputDevice("HostA", 1);

    initProvider("HostA");

    std::string reportedDevice;
    m_provider->usedOutputDeviceChanged().onReceive(nullptr, [&reportedDevice](const std::string& device) {
        reportedDevice = device;
    });

    m_deviceManager->pendingSystemChange = [this]() {
        m_deviceManager->setDefaultOutputDevice("HostA", 2);
    };
    m_devicesListener->systemDevicesChanged().notify();

    EXPECT_EQ(reportedDevice, "Headphones");
}

TEST_F(Au3AudioDevicesProviderTests, SystemDevicesChanged_UsedDevicesUnchanged_ReportsNothing)
{
    m_deviceManager->addInputDevice("HostA", "Mic A", 1, 2);
    m_deviceManager->setDefaultInputDevice("HostA", 1);
    m_deviceManager->addOutputDevice("HostA", "Speakers", 2);
    m_deviceManager->setDefaultOutputDevice("HostA", 2);

    initProvider("HostA");

    bool reported = false;
    m_provider->usedInputDeviceChanged().onReceive(nullptr, [&reported](const std::string&) { reported = true; });
    m_provider->usedOutputDeviceChanged().onReceive(nullptr, [&reported](const std::string&) { reported = true; });

    m_devicesListener->systemDevicesChanged().notify();

    EXPECT_EQ(m_deviceManager->rescanCount, 1);
    EXPECT_FALSE(reported);
}

TEST_F(Au3AudioDevicesProviderTests, SystemDevicesChanged_WhileStreamActive_DefersRescanUntilFinished)
{
    m_deviceManager->addInputDevice("HostA", "Mic A", 1, 2);
    m_deviceManager->setDefaultInputDevice("HostA", 1);

    initProvider("HostA");

    ON_CALL(*m_audioEngine, isBusy()).WillByDefault(::testing::Return(true));
    m_devicesListener->systemDevicesChanged().notify();

    EXPECT_EQ(m_deviceManager->rescanCount, 0);

    ON_CALL(*m_audioEngine, isBusy()).WillByDefault(::testing::Return(false));
    m_engineStreamStopped.notify();

    EXPECT_EQ(m_deviceManager->rescanCount, 1);
}

TEST_F(Au3AudioDevicesProviderTests, SetInputDevice_MultiSourceDevice_StoresRecordingSource)
{
    DeviceSourceMap& device = m_deviceManager->addInputDevice("HostA", "Mic A", 1, 2);
    device.totalSources = 2;
    device.sourceIndex = 1;
    device.sourceString = wxString("Line In");

    initProvider("HostA");

    // multi-source devices are listed as "<device>: <source>"
    m_provider->setInputDevice("Mic A: Line In");

    EXPECT_EQ(m_provider->currentInputDevice(), std::optional<std::string>("Mic A: Line In"));
    EXPECT_EQ(muse::settings()->value(RECORDING_SOURCE).toString(), "Line In");
}
}
