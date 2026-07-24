/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "audio/tests/mocks/audiodevicesprovidermock.h"
#include "playback/tests/mocks/playbackcontrollermock.h"

#include "../qml/Audacity/Preferences/commonaudioapiconfigurationmodel.h"

using ::testing::_;
using ::testing::An;
using ::testing::InSequence;
using ::testing::InvokeArgument;
using ::testing::NiceMock;
using ::testing::Return;

namespace au::appshell {
class CommonAudioApiConfigurationModelTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_model = std::make_unique<CommonAudioApiConfigurationModel>();

        m_provider = std::make_shared<NiceMock<audio::AudioDevicesProviderMock> >();
        m_model->audioDevicesProvider.set(m_provider);

        m_controller = std::make_shared<NiceMock<playback::PlaybackControllerMock> >();
        m_model->playbackController.set(m_controller);

        //! The applied state the dialog opens on
        ON_CALL(*m_provider, apis())
        .WillByDefault(Return(std::vector<std::string> { "CoreAudio", "JACK" }));
        ON_CALL(*m_provider, currentApi())
        .WillByDefault(Return(std::string("CoreAudio")));
        ON_CALL(*m_provider, outputDevices())
        .WillByDefault(Return(std::vector<std::string> { "Built-in Output", "Headphones" }));
        ON_CALL(*m_provider, currentOutputDevice())
        .WillByDefault(Return(std::string("Built-in Output")));
        ON_CALL(*m_provider, inputDevices())
        .WillByDefault(Return(std::vector<std::string> { "Built-in Mic" }));
        ON_CALL(*m_provider, currentInputDevice())
        .WillByDefault(Return(std::string("Built-in Mic")));
        ON_CALL(*m_provider, inputChannelsAvailable())
        .WillByDefault(Return(2));
        ON_CALL(*m_provider, inputChannelsSelected())
        .WillByDefault(Return(1));
        ON_CALL(*m_provider, bufferLength())
        .WillByDefault(Return(100.0));
        ON_CALL(*m_provider, latencyCompensation())
        .WillByDefault(Return(0.0));
        ON_CALL(*m_provider, defaultSampleRate())
        .WillByDefault(Return(44100));
        ON_CALL(*m_provider, defaultSampleFormat())
        .WillByDefault(Return(std::string("32-bit float")));

        //! The other API, previewed by some tests without being applied
        ON_CALL(*m_provider, outputDevices(An<const std::string&>()))
        .WillByDefault(Return(std::vector<std::string> { "JACK Out 1", "JACK Out 2" }));
        ON_CALL(*m_provider, inputDevices(An<const std::string&>()))
        .WillByDefault(Return(std::vector<std::string> { "JACK In" }));
        ON_CALL(*m_provider, inputChannelsAvailable(An<const std::string&>(), An<const std::string&>()))
        .WillByDefault(Return(4));

        //! The batch scope must run its payload for apply() to reach the setters
        ON_CALL(*m_controller, withSingleStreamRestart(_))
        .WillByDefault(InvokeArgument<0>());

        m_model->load();
    }

    std::unique_ptr<CommonAudioApiConfigurationModel> m_model;
    std::shared_ptr<audio::AudioDevicesProviderMock> m_provider;
    std::shared_ptr<playback::PlaybackControllerMock> m_controller;
};

/**
 * @brief Editing values in the dialog must not touch the audio backend.
 * @details The dialog edits pending values only; the stream, the monitoring
 *          and the stored settings stay untouched until OK. Closing the dialog
 *          without apply() (= Cancel) discards the edits — nothing may leak
 *          out on destruction either.
 */
TEST_F(CommonAudioApiConfigurationModelTests, EditingValues_DoesNotTouchTheAudioBackend)
{
    //! [THEN] Nothing reaches the playback controller or the provider setters
    EXPECT_CALL(*m_controller, withSingleStreamRestart(_)).Times(0);
    EXPECT_CALL(*m_controller, setAudioApi(_)).Times(0);
    EXPECT_CALL(*m_controller, setAudioOutputDevice(_)).Times(0);
    EXPECT_CALL(*m_controller, setAudioInputDevice(_)).Times(0);
    EXPECT_CALL(*m_controller, setInputChannels(_)).Times(0);
    EXPECT_CALL(*m_controller, setBufferLength(_)).Times(0);
    EXPECT_CALL(*m_controller, setLatencyCompensation(_)).Times(0);
    EXPECT_CALL(*m_provider, setDefaultSampleRate(_)).Times(0);
    EXPECT_CALL(*m_provider, setDefaultSampleFormat(_)).Times(0);
    EXPECT_CALL(*m_provider, setAutomaticCompensationEnabled(_)).Times(0);
    EXPECT_CALL(*m_provider, setAsioUseDeviceSampleRate(_)).Times(0);

    //! [WHEN] The user edits every setting in the dialog
    m_model->outputDeviceSelected("Headphones");
    m_model->inputChannelsSelected(1); // = 2 channels
    m_model->bufferLengthSelected("50");
    m_model->latencyCompensationSelected("10");
    m_model->defaultSampleRateValueSelected(48000);
    m_model->defaultSampleFormatSelected("16-bit");
    m_model->setAutomaticCompensationEnabled(true);

    //! [THEN] The dialog shows the edited values
    EXPECT_EQ(m_model->currentOutputDeviceId(), "Headphones");
    EXPECT_DOUBLE_EQ(m_model->bufferLength(), 50.0);
    EXPECT_EQ(m_model->defaultSampleRateValue(), 48000u);

    //! [WHEN] The dialog is cancelled (the model goes away without apply())
    m_model.reset();
}

/**
 * @brief OK pushes exactly the edited values, as one batch.
 * @details apply() must wrap the stream-affecting changes in a single
 *          withSingleStreamRestart() scope (one interruption for the whole
 *          dialog) and must not push values the user did not touch.
 */
TEST_F(CommonAudioApiConfigurationModelTests, Apply_PushesOnlyTheEditedValues_InOneBatch)
{
    //! [GIVEN] The user edited the output device, the channel count and the
    //! buffer length — nothing else
    m_model->outputDeviceSelected("Headphones");
    m_model->inputChannelsSelected(1); // = 2 channels
    m_model->bufferLengthSelected("50");

    //! [THEN] The untouched settings are not pushed
    EXPECT_CALL(*m_controller, setAudioApi(_)).Times(0);
    EXPECT_CALL(*m_controller, setAudioInputDevice(_)).Times(0);
    EXPECT_CALL(*m_controller, setLatencyCompensation(_)).Times(0);
    EXPECT_CALL(*m_provider, setDefaultSampleRate(_)).Times(0);
    EXPECT_CALL(*m_provider, setDefaultSampleFormat(_)).Times(0);

    //! [THEN] The edited ones are pushed inside a single batch scope
    InSequence seq;
    EXPECT_CALL(*m_controller, withSingleStreamRestart(_)).Times(1);
    EXPECT_CALL(*m_controller, setAudioOutputDevice("Headphones")).Times(1);
    EXPECT_CALL(*m_controller, setInputChannels(2)).Times(1);
    EXPECT_CALL(*m_controller, setBufferLength(50.0)).Times(1);

    //! [WHEN] The dialog is confirmed
    m_model->apply();
}

/**
 * @brief OK without any edits pushes nothing.
 */
TEST_F(CommonAudioApiConfigurationModelTests, Apply_WithoutEdits_PushesNothing)
{
    //! [THEN] No setter is called at all, and the stream restart scope itself
    //! is skipped — even an empty batch must not reach the controller
    EXPECT_CALL(*m_controller, withSingleStreamRestart(_)).Times(0);
    EXPECT_CALL(*m_controller, setAudioApi(_)).Times(0);
    EXPECT_CALL(*m_controller, setAudioOutputDevice(_)).Times(0);
    EXPECT_CALL(*m_controller, setAudioInputDevice(_)).Times(0);
    EXPECT_CALL(*m_controller, setInputChannels(_)).Times(0);
    EXPECT_CALL(*m_controller, setBufferLength(_)).Times(0);
    EXPECT_CALL(*m_controller, setLatencyCompensation(_)).Times(0);
    EXPECT_CALL(*m_provider, setDefaultSampleRate(_)).Times(0);
    EXPECT_CALL(*m_provider, setDefaultSampleFormat(_)).Times(0);
    EXPECT_CALL(*m_provider, setAutomaticCompensationEnabled(_)).Times(0);

    //! [WHEN] The dialog is confirmed unedited
    m_model->apply();
}

/**
 * @brief A value edited back to its applied state counts as unchanged.
 */
TEST_F(CommonAudioApiConfigurationModelTests, EditBackToApplied_IsNotPushed)
{
    //! [GIVEN] The user picked another device, then went back to the applied one
    m_model->outputDeviceSelected("Headphones");
    m_model->outputDeviceSelected("Built-in Output");

    //! [THEN] Nothing is pushed on OK
    EXPECT_CALL(*m_controller, setAudioOutputDevice(_)).Times(0);

    //! [WHEN] The dialog is confirmed
    m_model->apply();
}

/**
 * @brief Selecting another API previews that API's devices without applying.
 * @details The lists must repopulate from the per-API queries and a sensible
 *          device must be preselected (mirroring the fallback the backend will
 *          use on apply), while the audio engine stays untouched.
 */
TEST_F(CommonAudioApiConfigurationModelTests, ApiEdit_PreviewsTheNewApisDevices_WithoutApplying)
{
    //! [THEN] The preview does not reach the backend
    EXPECT_CALL(*m_controller, setAudioApi(_)).Times(0);
    EXPECT_CALL(*m_controller, setAudioOutputDevice(_)).Times(0);
    EXPECT_CALL(*m_controller, setAudioInputDevice(_)).Times(0);

    //! [WHEN] The user selects the other API
    m_model->setCurrentAudioApiIndex(1);

    //! [THEN] The dialog shows the pending API and its devices
    EXPECT_EQ(m_model->currentAudioApiIndex(), 1);
    const QVariantList expectedOutputs { QString("JACK Out 1"), QString("JACK Out 2") };
    EXPECT_EQ(m_model->outputDeviceList(), expectedOutputs);
    //! The applied devices belong to the old API, so the new API's first
    //! devices are preselected
    EXPECT_EQ(m_model->currentOutputDeviceId(), "JACK Out 1");
    EXPECT_EQ(m_model->currentInputDeviceId(), "JACK In");
    //! And the channel list follows the pending API/device
    EXPECT_EQ(m_model->inputChannelsList().size(), 4);
}

/**
 * @brief An API without devices previews empty selections, not old-API devices.
 * @details The applied devices belong to the old API and cannot survive the
 *          switch, so the dialog must not keep showing them.
 */
TEST_F(CommonAudioApiConfigurationModelTests, ApiEdit_NewApiHasNoDevices_PreviewsBlankDevices)
{
    //! [GIVEN] The other API has no devices at all
    ON_CALL(*m_provider, outputDevices(An<const std::string&>()))
    .WillByDefault(Return(std::vector<std::string> {}));
    ON_CALL(*m_provider, inputDevices(An<const std::string&>()))
    .WillByDefault(Return(std::vector<std::string> {}));

    //! [WHEN] The user selects it
    m_model->setCurrentAudioApiIndex(1);

    //! [THEN] The device lists and selections are empty
    EXPECT_TRUE(m_model->outputDeviceList().isEmpty());
    EXPECT_TRUE(m_model->inputDeviceList().isEmpty());
    EXPECT_EQ(m_model->currentOutputDeviceId(), "");
    EXPECT_EQ(m_model->currentInputDeviceId(), "");
}

/**
 * @brief OK after an API edit applies the API before the device chosen under it.
 */
TEST_F(CommonAudioApiConfigurationModelTests, Apply_AfterApiEdit_AppliesApiBeforeDevices)
{
    //! [GIVEN] The user selected the other API, then one of its devices
    m_model->setCurrentAudioApiIndex(1);
    m_model->outputDeviceSelected("JACK Out 2");

    //! [THEN] The API is applied first, then the devices that belong to it
    InSequence seq;
    EXPECT_CALL(*m_controller, withSingleStreamRestart(_)).Times(1);
    EXPECT_CALL(*m_controller, setAudioApi("JACK")).Times(1);
    EXPECT_CALL(*m_controller, setAudioOutputDevice("JACK Out 2")).Times(1);
    EXPECT_CALL(*m_controller, setAudioInputDevice("JACK In")).Times(1);

    //! [WHEN] The dialog is confirmed
    m_model->apply();
}

/**
 * @brief Switching the API back to the applied one drops the whole preview.
 * @details The device preselection made for the other API must not survive as
 *          a pending change — OK afterwards is a no-op.
 */
TEST_F(CommonAudioApiConfigurationModelTests, ApiEditBackToApplied_DropsTheDevicePreview)
{
    //! [GIVEN] The user visited the other API, then came back
    m_model->setCurrentAudioApiIndex(1);
    m_model->setCurrentAudioApiIndex(0);

    //! [THEN] The dialog shows the applied state again
    EXPECT_EQ(m_model->currentAudioApiIndex(), 0);
    EXPECT_EQ(m_model->currentOutputDeviceId(), "Built-in Output");

    //! [THEN] Nothing is pushed on OK
    EXPECT_CALL(*m_controller, setAudioApi(_)).Times(0);
    EXPECT_CALL(*m_controller, setAudioOutputDevice(_)).Times(0);
    EXPECT_CALL(*m_controller, setAudioInputDevice(_)).Times(0);

    //! [WHEN] The dialog is confirmed
    m_model->apply();
}

/**
 * @brief The non-stream settings go straight to the provider on OK.
 * @details Sample rate, sample format, automatic compensation and the ASIO
 *          flag don't interrupt a running stream, so they bypass the playback
 *          controller (the provider itself restores input monitoring where
 *          needed, e.g. for the sample rate).
 */
TEST_F(CommonAudioApiConfigurationModelTests, Apply_NonStreamSettings_GoDirectlyToTheProvider)
{
    //! [GIVEN] The user edited the sample rate, the format and the automatic
    //! compensation flag
    m_model->defaultSampleRateValueSelected(48000);
    m_model->defaultSampleFormatSelected("16-bit");
    m_model->setAutomaticCompensationEnabled(true);

    //! [THEN] They are pushed to the provider, not through the stream restart —
    //! the restart scope itself is skipped, so the stream is never interrupted
    EXPECT_CALL(*m_controller, withSingleStreamRestart(_)).Times(0);
    EXPECT_CALL(*m_provider, setDefaultSampleRate(48000)).Times(1);
    EXPECT_CALL(*m_provider, setDefaultSampleFormat("16-bit")).Times(1);
    EXPECT_CALL(*m_provider, setAutomaticCompensationEnabled(true)).Times(1);
    EXPECT_CALL(*m_controller, setAudioApi(_)).Times(0);
    EXPECT_CALL(*m_controller, setAudioOutputDevice(_)).Times(0);
    EXPECT_CALL(*m_controller, setBufferLength(_)).Times(0);

    //! [WHEN] The dialog is confirmed
    m_model->apply();
}

/**
 * @brief Changing the input device resets the pending channel selection.
 * @details A channel count picked for one device is meaningless on another;
 *          the selection falls back to the applied one, clamped to what the
 *          new device offers.
 */
TEST_F(CommonAudioApiConfigurationModelTests, InputDeviceEdit_ResetsThePendingChannelCount)
{
    //! [GIVEN] Another input device exists alongside the applied one
    ON_CALL(*m_provider, inputDevices())
    .WillByDefault(Return(std::vector<std::string> { "Built-in Mic", "USB Interface" }));

    //! [GIVEN] The user picked 2 channels, then another input device
    m_model->inputChannelsSelected(1); // = 2 channels
    m_model->inputDeviceSelected("USB Interface");

    //! [THEN] Only the device change is pushed on OK — the stale channel
    //! selection was dropped with the device it was made for
    EXPECT_CALL(*m_controller, setAudioInputDevice("USB Interface")).Times(1);
    EXPECT_CALL(*m_controller, setInputChannels(_)).Times(0);

    //! [WHEN] The dialog is confirmed
    m_model->apply();
}
}
