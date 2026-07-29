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
constexpr const char* SYSTEM_DEFAULT = "System default";

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
        ON_CALL(*m_controller, inputChannelsAvailable("JACK", _)).WillByDefault(Return(4));
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

    m_model->outputDeviceSelected(2);
    m_model->inputChannelsSelected(1);
    m_model->bufferLengthSelected("50");

    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 2);
    EXPECT_TRUE(m_model->currentInputChannelsSelected().startsWith("2"));
    EXPECT_DOUBLE_EQ(m_model->bufferLength(), 50.0);
}

TEST_F(CommonAudioApiConfigurationModelTests, Apply_SubmitsAllPendingFieldsInOneChangeSet)
{
    m_model->outputDeviceSelected(2);
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
    m_model->outputDeviceSelected(2);
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
    // The applied devices do not exist under JACK, so both fall back to the system default.
    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 0);
    EXPECT_EQ(m_model->currentInputDeviceIndex(), 0);
    EXPECT_EQ(m_model->outputDeviceList().size(), 3);
    EXPECT_EQ(m_model->inputChannelsList().size(), 4);
}

TEST_F(CommonAudioApiConfigurationModelTests, EditingBackToAppliedStateProducesAnEmptyChange)
{
    m_model->outputDeviceSelected(2);
    m_model->outputDeviceSelected(1);

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
    m_model->outputDeviceSelected(2);
    m_applied.outputDevice = "External Output";
    audio::AudioConfigurationDelta delta;
    delta.fields = audio::fieldMask(audio::AudioConfigurationField::OutputDevice);

    m_configurationChanged.send(delta);

    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 2);
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
    m_model->outputDeviceSelected(2);

    m_model->reset();

    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 1);
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

TEST_F(CommonAudioApiConfigurationModelTests, DeviceLists_StartWithTheSystemDefaultEntry)
{
    const QVariantList outputs = m_model->outputDeviceList();
    ASSERT_EQ(outputs.size(), 3);
    EXPECT_EQ(outputs.at(0).toString(), QString(SYSTEM_DEFAULT));
    EXPECT_EQ(outputs.at(1).toString(), QString("Built-in Output"));
    EXPECT_EQ(outputs.at(2).toString(), QString("Headphones"));

    const QVariantList inputs = m_model->inputDeviceList();
    ASSERT_EQ(inputs.size(), 3);
    EXPECT_EQ(inputs.at(0).toString(), QString(SYSTEM_DEFAULT));
    EXPECT_EQ(inputs.at(1).toString(), QString("Built-in Mic"));
    EXPECT_EQ(inputs.at(2).toString(), QString("USB Mic"));
}

TEST_F(CommonAudioApiConfigurationModelTests, DeviceLists_NoDevices_AreEmptyAndHaveNoCurrentIndex)
{
    ON_CALL(*m_controller, outputDevices("Core Audio"))
    .WillByDefault(Return(std::vector<std::string> {}));
    ON_CALL(*m_controller, inputDevices("Core Audio"))
    .WillByDefault(Return(std::vector<std::string> {}));

    EXPECT_TRUE(m_model->outputDeviceList().isEmpty());
    EXPECT_EQ(m_model->currentOutputDeviceIndex(), -1);
    EXPECT_TRUE(m_model->inputDeviceList().isEmpty());
    EXPECT_EQ(m_model->currentInputDeviceIndex(), -1);
}

TEST_F(CommonAudioApiConfigurationModelTests, CurrentDeviceIndex_IsShiftedByTheSystemDefaultEntry)
{
    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 1);
    EXPECT_EQ(m_model->currentInputDeviceIndex(), 1);

    m_applied.outputDevice = std::nullopt;
    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 0);

    m_applied.outputDevice = "Unplugged device";
    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 0);
}

TEST_F(CommonAudioApiConfigurationModelTests, DeviceSelected_SystemDefaultEntry_StagesTheDefaultSelection)
{
    m_model->outputDeviceSelected(0);

    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 0);
    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce([](const muse::modularity::ContextPtr&, const audio::AudioConfigurationChange& change) {
        EXPECT_EQ(change.outputDevice, std::optional<audio::AudioDeviceSelection>(audio::AudioDeviceSelection {}));
        return audio::ApplyResult { audio::ApplyStatus::Applied };
    });

    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests, DeviceSelected_OutOfRange_IsIgnored)
{
    m_model->outputDeviceSelected(3);
    m_model->outputDeviceSelected(-1);
    m_model->inputDeviceSelected(3);
    m_model->inputDeviceSelected(-1);

    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 1);
    EXPECT_EQ(m_model->currentInputDeviceIndex(), 1);
    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce([](const muse::modularity::ContextPtr&, const audio::AudioConfigurationChange& change) {
        EXPECT_FALSE(change.outputDevice);
        EXPECT_FALSE(change.inputDevice);
        return audio::ApplyResult { audio::ApplyStatus::NoChange };
    });

    EXPECT_TRUE(m_model->apply());
}

//! NOTE A real device may carry the same name as the "System default" entry;
//! index-based selection must keep the two distinguishable
TEST_F(CommonAudioApiConfigurationModelTests, DeviceNamedSystemDefault_IsDistinctFromTheDefaultEntry)
{
    ON_CALL(*m_controller, outputDevices("Core Audio"))
    .WillByDefault(Return(std::vector<std::string> { SYSTEM_DEFAULT }));
    m_applied.outputDevice = SYSTEM_DEFAULT;

    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 1);

    m_model->outputDeviceSelected(0);

    EXPECT_EQ(m_model->currentOutputDeviceIndex(), 0);
    EXPECT_CALL(*m_controller, apply(_, _))
    .WillOnce([](const muse::modularity::ContextPtr&, const audio::AudioConfigurationChange& change) {
        EXPECT_EQ(change.outputDevice, std::optional<audio::AudioDeviceSelection>(audio::AudioDeviceSelection {}));
        return audio::ApplyResult { audio::ApplyStatus::Applied };
    });

    EXPECT_TRUE(m_model->apply());
}

TEST_F(CommonAudioApiConfigurationModelTests, DeviceLists_ShowTheResolvedSystemDefaultName)
{
    ON_CALL(*m_controller, systemDefaultOutputDevice("Core Audio"))
    .WillByDefault(Return(std::string("Headphones")));
    ON_CALL(*m_controller, systemDefaultInputDevice("Core Audio"))
    .WillByDefault(Return(std::string("USB Mic")));

    EXPECT_EQ(m_model->outputDeviceList().at(0).toString(), QString("System default: Headphones"));
    EXPECT_EQ(m_model->inputDeviceList().at(0).toString(), QString("System default: USB Mic"));
}

TEST_F(CommonAudioApiConfigurationModelTests, DeviceLists_ResolvedSystemDefaultFollowsThePreviewedApi)
{
    ON_CALL(*m_controller, systemDefaultOutputDevice("JACK"))
    .WillByDefault(Return(std::string("JACK Out 2")));

    m_model->setCurrentAudioApiIndex(1);

    EXPECT_EQ(m_model->outputDeviceList().at(0).toString(), QString("System default: JACK Out 2"));
}

TEST_F(CommonAudioApiConfigurationModelTests, ExternalDeviceChangeIsForwardedAsAnIndexChangeSignal)
{
    int outputChangedCount = 0;
    int inputChangedCount = 0;
    QObject::connect(m_model.get(), &CommonAudioApiConfigurationModel::currentOutputDeviceIndexChanged,
                     m_model.get(), [&outputChangedCount]() { ++outputChangedCount; });
    QObject::connect(m_model.get(), &CommonAudioApiConfigurationModel::currentInputDeviceIndexChanged,
                     m_model.get(), [&inputChangedCount]() { ++inputChangedCount; });

    audio::AudioConfigurationDelta delta;
    delta.fields = audio::fieldMask(audio::AudioConfigurationField::OutputDevice);
    m_configurationChanged.send(delta);

    EXPECT_EQ(outputChangedCount, 1);
    EXPECT_EQ(inputChangedCount, 1);
}
}
