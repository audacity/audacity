/*
 * Audacity: A Digital Audio Editor
 */
#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include "audio/tests/mocks/audiodrivercontrollermock.h"

#include "../qml/Audacity/Preferences/commonaudioapiconfigurationmodel.h"

using ::testing::_;
using ::testing::NiceMock;
using ::testing::Return;

namespace au::appshell {
class CommonAudioApiConfigurationModelTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_context = std::make_shared<muse::modularity::Context>(201);
        m_model = std::make_unique<CommonAudioApiConfigurationModel>();
        m_model->setContext(m_context);
        m_controller = std::make_shared<NiceMock<audio::AudioDriverControllerMock> >();
        m_model->audioDriverController.set(m_controller);

        m_applied.api = "Core Audio";
        m_applied.outputDevice = "Built-in Output";
        m_applied.inputDevice = "Built-in Mic";
        m_applied.inputChannels = 1;
        m_applied.bufferLength = 100.0;
        m_applied.defaultSampleRate = 44100;
        m_applied.defaultSampleFormat = "32-bit float";

        ON_CALL(*m_controller, configuration()).WillByDefault([this]() { return m_applied; });
        ON_CALL(*m_controller, configurationChanged())
            .WillByDefault(Return(m_configurationChanged));
        ON_CALL(*m_controller, audioDeviceListChanged())
            .WillByDefault(Return(m_deviceListChanged));
        ON_CALL(*m_controller, apis())
        .WillByDefault(Return(std::vector<std::string> { "Core Audio", "JACK" }));
        ON_CALL(*m_controller, outputDevices())
        .WillByDefault(Return(std::vector<std::string> { "Built-in Output", "Headphones" }));
        ON_CALL(*m_controller, inputDevices())
        .WillByDefault(Return(std::vector<std::string> { "Built-in Mic", "USB Mic" }));
        ON_CALL(*m_controller, inputChannelsAvailable()).WillByDefault(Return(2));
        ON_CALL(*m_controller, outputDevices("Core Audio"))
        .WillByDefault(Return(std::vector<std::string> { "Built-in Output", "Headphones" }));
        ON_CALL(*m_controller, inputDevices("Core Audio"))
        .WillByDefault(Return(std::vector<std::string> { "Built-in Mic", "USB Mic" }));
        ON_CALL(*m_controller, outputDevices("JACK"))
        .WillByDefault(Return(std::vector<std::string> { "JACK Out 1", "JACK Out 2" }));
        ON_CALL(*m_controller, inputDevices("JACK"))
        .WillByDefault(Return(std::vector<std::string> { "JACK In" }));
        ON_CALL(*m_controller, inputChannelsAvailable("JACK", "JACK In")).WillByDefault(Return(4));
        ON_CALL(*m_controller, sampleRates())
        .WillByDefault(Return(std::vector<uint64_t> { 44100, 48000 }));

        m_model->load();
    }

    void TearDown() override
    {
        m_model.reset();
        muse::modularity::removeIoC(m_context);
    }

    std::unique_ptr<CommonAudioApiConfigurationModel> m_model;
    muse::modularity::ContextPtr m_context;
    std::shared_ptr<NiceMock<audio::AudioDriverControllerMock> > m_controller;
    audio::AudioConfiguration m_applied;
    muse::async::Channel<audio::AudioConfigurationDelta> m_configurationChanged;
    muse::async::Notification m_deviceListChanged;
};

TEST_F(CommonAudioApiConfigurationModelTests, EditingValues_IsPendingUntilApply)
{
    EXPECT_CALL(*m_controller, apply(_, _)).Times(0);

    m_model->outputDeviceSelected("Headphones");
    m_model->inputChannelsSelected(1);
    m_model->bufferLengthSelected("50");

    EXPECT_EQ(m_model->currentOutputDeviceId(), "Headphones");
    EXPECT_TRUE(m_model->currentInputChannelsSelected().startsWith("2"));
    EXPECT_DOUBLE_EQ(m_model->bufferLength(), 50.0);
}

TEST_F(CommonAudioApiConfigurationModelTests, Apply_SubmitsAllPendingFieldsInOneChangeSet)
{
    m_model->outputDeviceSelected("Headphones");
    m_model->inputChannelsSelected(1);
    m_model->bufferLengthSelected("50");

    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce([](const muse::modularity::ContextPtr&, const audio::AudioConfigurationChange& change) {
        EXPECT_EQ(change.outputDevice, std::optional<std::string>("Headphones"));
        EXPECT_EQ(change.inputChannels, std::optional<int>(2));
        EXPECT_EQ(change.bufferLength, std::optional<double>(50.0));
        EXPECT_FALSE(change.api.has_value());
        EXPECT_FALSE(change.inputDevice.has_value());
        return audio::ApplyResult { audio::ApplyStatus::Applied };
    });

    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests, ApplyFailure_KeepsPendingValuesAndDialogOpen)
{
    m_model->bufferLengthSelected("50");
    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce(Return(audio::ApplyResult { audio::ApplyStatus::InternalError }));

    EXPECT_FALSE(m_model->apply());
    EXPECT_DOUBLE_EQ(m_model->bufferLength(), 50.0);
}

TEST_F(CommonAudioApiConfigurationModelTests, ApplySuccess_ClearsFieldsThatNormalizedToTheCurrentValue)
{
    m_model->outputDeviceSelected("Headphones");
    m_model->bufferLengthSelected("50");
    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce(Return(audio::ApplyResult { audio::ApplyStatus::Applied }))
    .WillOnce([](const muse::modularity::ContextPtr&, const audio::AudioConfigurationChange& change) {
        EXPECT_FALSE(change.api);
        EXPECT_FALSE(change.outputDevice);
        EXPECT_FALSE(change.inputDevice);
        EXPECT_FALSE(change.inputChannels);
        EXPECT_FALSE(change.bufferLength);
        EXPECT_FALSE(change.automaticLatencyCompensation);
        EXPECT_FALSE(change.latencyCompensation);
        EXPECT_FALSE(change.defaultSampleRate);
        EXPECT_FALSE(change.defaultSampleFormat);
        EXPECT_FALSE(change.asioUseDeviceSampleRate);
        return audio::ApplyResult { audio::ApplyStatus::NoChange };
    });

    EXPECT_TRUE(m_model->apply());
    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests, ApiEdit_PreviewsTheSelectedApisDeviceList)
{
    EXPECT_CALL(*m_controller, apply(_, _)).Times(0);

    m_model->setCurrentAudioApiIndex(1);

    EXPECT_EQ(m_model->currentAudioApiIndex(), 1);
    EXPECT_EQ(m_model->currentOutputDeviceId(), "JACK Out 1");
    EXPECT_EQ(m_model->currentInputDeviceId(), "JACK In");
    EXPECT_EQ(m_model->inputChannelsList().size(), 4);
}

TEST_F(CommonAudioApiConfigurationModelTests, EditingBackToAppliedStateProducesAnEmptyChange)
{
    m_model->outputDeviceSelected("Headphones");
    m_model->outputDeviceSelected("Built-in Output");

    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce([](const muse::modularity::ContextPtr&, const audio::AudioConfigurationChange& change) {
        EXPECT_FALSE(change.api);
        EXPECT_FALSE(change.outputDevice);
        EXPECT_FALSE(change.inputDevice);
        EXPECT_FALSE(change.inputChannels);
        EXPECT_FALSE(change.bufferLength);
        EXPECT_FALSE(change.automaticLatencyCompensation);
        EXPECT_FALSE(change.latencyCompensation);
        EXPECT_FALSE(change.defaultSampleRate);
        EXPECT_FALSE(change.defaultSampleFormat);
        EXPECT_FALSE(change.asioUseDeviceSampleRate);
        return audio::ApplyResult { audio::ApplyStatus::NoChange };
    });

    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests,
    ExternalChangeDoesNotDiscardPendingEdit)
{
    m_model->outputDeviceSelected("Headphones");
    m_applied.outputDevice = "External Output";
    audio::AudioConfigurationDelta delta;
    delta.fields = audio::fieldMask(audio::AudioConfigurationField::OutputDevice);

    m_configurationChanged.send(delta);

    EXPECT_EQ(m_model->currentOutputDeviceId(), "Headphones");
    EXPECT_CALL(*m_controller, apply(_, _))
        .WillOnce([](const muse::modularity::ContextPtr&,
                      const audio::AudioConfigurationChange& change) {
            EXPECT_EQ(change.outputDevice,
                std::optional<std::string>("Headphones"));
            return audio::ApplyResult { audio::ApplyStatus::Applied };
        });
    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests, ResetDiscardsPendingEdits)
{
    m_model->outputDeviceSelected("Headphones");

    m_model->reset();

    EXPECT_EQ(m_model->currentOutputDeviceId(), "Built-in Output");
    EXPECT_CALL(*m_controller, apply(_, _))
        .WillOnce([](const muse::modularity::ContextPtr&,
                      const audio::AudioConfigurationChange& change) {
            EXPECT_FALSE(change.outputDevice);
            return audio::ApplyResult { audio::ApplyStatus::NoChange };
        });
    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests, ExternalSampleRateChangeUpdatesOtherRateState)
{
    audio::AudioConfigurationDelta delta;
    delta.fields = audio::fieldMask(audio::AudioConfigurationField::DefaultSampleRate);

    m_applied.defaultSampleRate = 12345;
    m_configurationChanged.send(delta);
    EXPECT_TRUE(m_model->otherSampleRate());

    m_applied.defaultSampleRate = 48000;
    m_configurationChanged.send(delta);
    EXPECT_FALSE(m_model->otherSampleRate());
}

TEST_F(CommonAudioApiConfigurationModelTests, RestoreFailureIsAnAppliedResult)
{
    m_model->bufferLengthSelected("50");
    EXPECT_CALL(*m_controller, apply(_, _))
        .WillOnce(Return(audio::ApplyResult {
            audio::ApplyStatus::Applied,
            true }));

    EXPECT_TRUE(m_model->apply());
}
}
