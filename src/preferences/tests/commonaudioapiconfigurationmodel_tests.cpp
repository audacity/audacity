/*
* Audacity: A Digital Audio Editor
*/
#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <memory>
#include <string>
#include <vector>

#include <QString>

#include "actions/tests/mocks/actionsdispatchermock.h"
#include "framework/global/modularity/ioc.h"
#include "preferences/qml/Audacity/Preferences/commonaudioapiconfigurationmodel.h"

using ::testing::A;
using ::testing::Invoke;
using ::testing::NiceMock;

namespace {
class FakeAudioDevicesProvider final : public au::audio::IAudioDevicesProvider
{
public:
    std::vector<std::string> outputDevices() const override { return outputDevicesList; }
    std::string currentOutputDevice() const override { return currentOutput; }
    void setOutputDevice(const std::string& device) override
    {
        lastOutputDevice = device;
        ++setOutputDeviceCalls;
    }

    muse::async::Notification outputDeviceChanged() const override { return {}; }

    std::vector<std::string> inputDevices() const override { return inputDevicesList; }
    std::string currentInputDevice() const override { return currentInput; }
    void setInputDevice(const std::string& device) override
    {
        lastInputDevice = device;
        ++setInputDeviceCalls;
    }

    muse::async::Notification inputDeviceChanged() const override { return {}; }

    std::vector<std::string> apis() const override { return apisList; }
    std::string currentApi() const override { return currentApiName; }
    void setApi(const std::string& api) override
    {
        lastApi = api;
        ++setApiCalls;
    }

    muse::async::Notification apiChanged() const override { return {}; }

    int inputChannelsAvailable() const override { return inputChannelsAvailableValue; }
    int inputChannelsSelected() const override { return inputChannelsSelectedValue; }
    void setInputChannels(const int count) override
    {
        lastInputChannels = count;
        ++setInputChannelsCalls;
    }

    muse::async::Notification inputChannelsAvailableChanged() const override { return {}; }
    muse::async::Notification inputChannelsChanged() const override { return {}; }

    double bufferLength() const override { return bufferLengthValue; }
    void setBufferLength(double newBufferLength) override
    {
        lastBufferLength = newBufferLength;
        ++setBufferLengthCalls;
    }

    muse::async::Notification bufferLengthChanged() const override { return {}; }

    double latencyCompensation() const override { return latencyCompensationValue; }
    void setLatencyCompensation(double newLatencyCompensation) override
    {
        lastLatencyCompensation = newLatencyCompensation;
        ++setLatencyCompensationCalls;
    }

    muse::async::Notification latencyCompensationChanged() const override { return {}; }

    std::vector<uint64_t> sampleRates() const override { return sampleRatesList; }
    uint64_t defaultSampleRate() const override { return defaultSampleRateValue; }
    void setDefaultSampleRate(uint64_t newRate) override
    {
        lastSampleRate = newRate;
        ++setDefaultSampleRateCalls;
    }

    muse::async::Notification defaultSampleRateChanged() const override { return {}; }

    std::vector<std::string> sampleFormats() const override { return sampleFormatsList; }
    std::string defaultSampleFormat() const override { return defaultSampleFormatValue; }
    void setDefaultSampleFormat(const std::string& format) override
    {
        lastSampleFormat = format;
        ++setDefaultSampleFormatCalls;
    }

    muse::async::Notification defaultSampleFormatChanged() const override { return {}; }

    void handleDeviceChange() override { ++handleDeviceChangeCalls; }
    void rescan() override { ++rescanCalls; }

    std::vector<std::string> outputDevicesList;
    std::vector<std::string> inputDevicesList;
    std::vector<std::string> apisList;
    std::string currentOutput;
    std::string currentInput;
    std::string currentApiName;
    int inputChannelsAvailableValue = 0;
    int inputChannelsSelectedValue = 0;
    double bufferLengthValue = 0.0;
    double latencyCompensationValue = 0.0;
    std::vector<uint64_t> sampleRatesList;
    uint64_t defaultSampleRateValue = 0;
    std::vector<std::string> sampleFormatsList;
    std::string defaultSampleFormatValue;

    int setOutputDeviceCalls = 0;
    int setInputDeviceCalls = 0;
    int setApiCalls = 0;
    int setInputChannelsCalls = 0;
    int setBufferLengthCalls = 0;
    int setLatencyCompensationCalls = 0;
    int setDefaultSampleRateCalls = 0;
    int setDefaultSampleFormatCalls = 0;
    int handleDeviceChangeCalls = 0;
    int rescanCalls = 0;

    std::string lastOutputDevice;
    std::string lastInputDevice;
    std::string lastApi;
    int lastInputChannels = 0;
    double lastBufferLength = 0.0;
    double lastLatencyCompensation = 0.0;
    uint64_t lastSampleRate = 0;
    std::string lastSampleFormat;
};

bool matchesActionQuery(const muse::actions::ActionQuery& query,
                        const std::string& uri,
                        const std::string& paramKey,
                        int paramValue)
{
    return query.uri().toString() == uri
           && query.param(paramKey).toInt() == paramValue;
}
}

namespace au::appshell {
class CommonAudioApiConfigurationModelTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        dispatcher = std::make_shared<NiceMock<muse::actions::ActionsDispatcherMock> >();
        audioDevicesProvider = std::make_shared<FakeAudioDevicesProvider>();

        muse::modularity::globalIoc()->registerExport<muse::actions::IActionsDispatcher>("utests", dispatcher);
        muse::modularity::globalIoc()->registerExport<au::audio::IAudioDevicesProvider>("utests", audioDevicesProvider);

        model = std::make_unique<CommonAudioApiConfigurationModel>();
    }

    void TearDown() override
    {
        model.reset();
        muse::modularity::globalIoc()->unregister<muse::actions::IActionsDispatcher>("utests");
        muse::modularity::globalIoc()->unregister<au::audio::IAudioDevicesProvider>("utests");
        dispatcher.reset();
        audioDevicesProvider.reset();
    }

    std::shared_ptr<NiceMock<muse::actions::ActionsDispatcherMock> > dispatcher;
    std::shared_ptr<FakeAudioDevicesProvider> audioDevicesProvider;
    std::unique_ptr<CommonAudioApiConfigurationModel> model;
};

TEST_F(CommonAudioApiConfigurationModelTests, SetCurrentAudioApiIndexDispatchesAction)
{
    audioDevicesProvider->apisList = { "ALSA", "CoreAudio" };
    audioDevicesProvider->currentApiName = "ALSA";

    EXPECT_CALL(*dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(Invoke([](const muse::actions::ActionQuery& query) {
        EXPECT_TRUE(matchesActionQuery(query, "action://playback/change-api", "api_index", 1));
    }));

    model->setCurrentAudioApiIndex(1);

    EXPECT_EQ(audioDevicesProvider->setApiCalls, 0);
}

TEST_F(CommonAudioApiConfigurationModelTests, OutputDeviceSelectedDispatchesAction)
{
    audioDevicesProvider->outputDevicesList = { "System default", "Speakers" };
    audioDevicesProvider->currentOutput = "System default";

    EXPECT_CALL(*dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(Invoke([](const muse::actions::ActionQuery& query) {
        EXPECT_TRUE(matchesActionQuery(query, "action://playback/change-playback-device", "device_index", 1));
    }));

    model->outputDeviceSelected(QString::fromUtf8("Speakers"));

    EXPECT_EQ(audioDevicesProvider->setOutputDeviceCalls, 0);
}

TEST_F(CommonAudioApiConfigurationModelTests, InputDeviceSelectedDispatchesAction)
{
    audioDevicesProvider->inputDevicesList = { "System default", "Mic" };
    audioDevicesProvider->currentInput = "System default";

    EXPECT_CALL(*dispatcher, dispatch(A<const muse::actions::ActionQuery&>()))
    .WillOnce(Invoke([](const muse::actions::ActionQuery& query) {
        EXPECT_TRUE(matchesActionQuery(query, "action://playback/change-recording-device", "device_index", 1));
    }));

    model->inputDeviceSelected(QString::fromUtf8("Mic"));

    EXPECT_EQ(audioDevicesProvider->setInputDeviceCalls, 0);
}
}
