/*
* Audacity: A Digital Audio Editor
*/
#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <memory>
#include <string>
#include <unordered_map>

#include <wx/string.h>

#include "au3audio/internal/au3audiodevicesprovider.h"
#include "au3audio/internal/au3devicemanageriface.h"
#include "framework/global/async/asyncable.h"
#include "framework/global/settings.h"
#include "types/translatablestring.h"

#include "mocks/audioenginemock.h"

using ::testing::NiceMock;

namespace {
static const muse::Settings::Key AUDIO_HOST("au3audio", "AudioIO/Host");
static const muse::Settings::Key PLAYBACK_DEVICE("au3audio", "AudioIO/PlaybackDevice");
static const muse::Settings::Key RECORDING_DEVICE("au3audio", "AudioIO/RecordingDevice");
static const muse::Settings::Key WRAP_PLAYBACK_DEVICE("au3wrap", "AudioIO/PlaybackDevice");
static const muse::Settings::Key WRAP_RECORDING_DEVICE("au3wrap", "AudioIO/RecordingDevice");

std::string systemDefaultSelection()
{
    return muse::qtrc("preferences", "System default").toStdString();
}

DeviceSourceMap makeDevice(int deviceIndex, int hostIndex, int numChannels,
                           const std::string& hostName, const std::string& deviceName,
                           const std::string& sourceName = std::string(),
                           int sourceIndex = 0, int totalSources = 1)
{
    DeviceSourceMap map{};
    map.deviceIndex = deviceIndex;
    map.sourceIndex = sourceIndex;
    map.hostIndex = hostIndex;
    map.totalSources = totalSources;
    map.numChannels = numChannels;
    map.sourceString = wxString::FromUTF8(sourceName);
    map.deviceString = wxString::FromUTF8(deviceName);
    map.hostString = wxString::FromUTF8(hostName);
    return map;
}

class FakeDeviceManager final : public au::au3audio::IAu3DeviceManager
{
public:
    const std::vector<DeviceSourceMap>& inputDeviceMaps() const override
    {
        return inputDevices;
    }

    const std::vector<DeviceSourceMap>& outputDeviceMaps() const override
    {
        return outputDevices;
    }

    int systemDefaultOutputDeviceIndex(const std::string& /*hostName*/) const override
    {
        return systemDefaultOutputIndex;
    }

    int systemDefaultInputDeviceIndex(const std::string& /*hostName*/) const override
    {
        return systemDefaultInputIndex;
    }

    int hostIndex(const std::string& hostName) const override
    {
        auto it = hostIndices.find(hostName);
        if (it == hostIndices.end()) {
            return -1;
        }
        return it->second;
    }

    DeviceSourceMap* defaultOutputDevice(int hostIndex) const override
    {
        auto it = defaultOutputs.find(hostIndex);
        if (it == defaultOutputs.end()) {
            return nullptr;
        }
        return it->second;
    }

    DeviceSourceMap* defaultInputDevice(int hostIndex) const override
    {
        auto it = defaultInputs.find(hostIndex);
        if (it == defaultInputs.end()) {
            return nullptr;
        }
        return it->second;
    }

    void rescan() override
    {
    }

    std::vector<DeviceSourceMap> inputDevices;
    std::vector<DeviceSourceMap> outputDevices;
    std::unordered_map<std::string, int> hostIndices;
    std::unordered_map<int, DeviceSourceMap*> defaultOutputs;
    std::unordered_map<int, DeviceSourceMap*> defaultInputs;
    int systemDefaultOutputIndex = -1;
    int systemDefaultInputIndex = -1;
};
}

namespace au::au3audio {
class Au3AudioDevicesProviderTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        muse::settings()->beginTransaction(false);

        audioEngine = std::make_shared<NiceMock<tests::AudioEngineMock> >();
        muse::modularity::globalIoc()->registerExport<au::audio::IAudioEngine>("utests", audioEngine);

        provider = std::make_unique<Au3AudioDevicesProvider>(muse::modularity::globalCtx());
        provider->setDeviceManagerForTests(&deviceManager);

        muse::settings()->setLocalValue(AUDIO_HOST, muse::Val(std::string()));
        muse::settings()->setLocalValue(PLAYBACK_DEVICE, muse::Val(std::string()));
        muse::settings()->setLocalValue(RECORDING_DEVICE, muse::Val(std::string()));
        muse::settings()->setLocalValue(WRAP_PLAYBACK_DEVICE, muse::Val(std::string()));
        muse::settings()->setLocalValue(WRAP_RECORDING_DEVICE, muse::Val(std::string()));
    }

    void TearDown() override
    {
        muse::modularity::globalIoc()->unregister<au::audio::IAudioEngine>("utests");
        audioEngine.reset();
        provider.reset();
        muse::settings()->rollbackTransaction(false);
    }

    FakeDeviceManager deviceManager;
    std::shared_ptr<NiceMock<tests::AudioEngineMock> > audioEngine;
    std::unique_ptr<Au3AudioDevicesProvider> provider;
};

TEST_F(Au3AudioDevicesProviderTests, OutputDevicesIncludeSystemDefaultAndCurrentSelectionIsLabelWhenUnset)
{
    const std::string host = "TestHost";
    deviceManager.hostIndices[host] = 0;
    deviceManager.outputDevices.push_back(makeDevice(10, 0, 2, host, "Speakers"));

    muse::settings()->setLocalValue(AUDIO_HOST, muse::Val(host));
    muse::settings()->setLocalValue(PLAYBACK_DEVICE, muse::Val(std::string()));

    provider->updateInputOutputDevices();

    const auto devices = provider->outputDevices();
    ASSERT_EQ(devices.size(), 2u);
    EXPECT_EQ(devices.front(), systemDefaultSelection());
    EXPECT_EQ(devices.back(), "Speakers");
    EXPECT_EQ(provider->currentOutputDevice(), systemDefaultSelection());
}

TEST_F(Au3AudioDevicesProviderTests, InputDevicesIncludeSystemDefaultAndCurrentSelectionIsLabelWhenUnset)
{
    const std::string host = "TestHost";
    deviceManager.hostIndices[host] = 0;
    deviceManager.inputDevices.push_back(makeDevice(11, 0, 2, host, "Mic"));

    muse::settings()->setLocalValue(AUDIO_HOST, muse::Val(host));
    muse::settings()->setLocalValue(RECORDING_DEVICE, muse::Val(std::string()));

    provider->updateInputOutputDevices();

    const auto devices = provider->inputDevices();
    ASSERT_EQ(devices.size(), 2u);
    EXPECT_EQ(devices.front(), systemDefaultSelection());
    EXPECT_EQ(devices.back(), "Mic");
    EXPECT_EQ(provider->currentInputDevice(), systemDefaultSelection());
}

TEST_F(Au3AudioDevicesProviderTests, SetOutputDeviceSystemDefaultClearsSettingsAndWrap)
{
    muse::settings()->setLocalValue(PLAYBACK_DEVICE, muse::Val(std::string("Speakers")));

    provider->setOutputDevice(systemDefaultSelection());

    EXPECT_TRUE(muse::settings()->value(PLAYBACK_DEVICE).toString().empty());
    EXPECT_TRUE(muse::settings()->value(WRAP_PLAYBACK_DEVICE).toString().empty());
}

TEST_F(Au3AudioDevicesProviderTests, SetInputDeviceSystemDefaultClearsSettingsAndWrap)
{
    muse::settings()->setLocalValue(RECORDING_DEVICE, muse::Val(std::string("Mic")));

    provider->setInputDevice(systemDefaultSelection());

    EXPECT_TRUE(muse::settings()->value(RECORDING_DEVICE).toString().empty());
    EXPECT_TRUE(muse::settings()->value(WRAP_RECORDING_DEVICE).toString().empty());
}
}
