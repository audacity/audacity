/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "mocks/audioenginemock.h"

#include "../internal/au3audiodevicesprovider.h"

using ::testing::InSequence;
using ::testing::NiceMock;
using ::testing::Return;

namespace au::au3audio {
class Au3AudioDevicesProviderTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_provider = std::make_shared<Au3AudioDevicesProvider>(muse::modularity::globalCtx());

        m_audioEngine = std::make_shared<NiceMock<audio::AudioEngineMock> >();
        m_provider->audioEngine.set(m_audioEngine);
    }

    void handleDeviceChange()
    {
        m_provider->handleDeviceChange();
    }

    std::shared_ptr<Au3AudioDevicesProvider> m_provider;
    std::shared_ptr<audio::AudioEngineMock> m_audioEngine;
};

/**
 * @brief Changing the device while a stream is open must stop it first.
 * @details AudioIOBase::HandleDeviceChange() asserts on an active stream
 *          (issue #11098). A playing/paused/recording stream is reported by
 *          isBusy(), so it must be stopped before the low-level switch — and
 *          before it, in that order.
 */
TEST_F(Au3AudioDevicesProviderTests, HandleDeviceChange_WhenBusy_StopsStreamBeforeSwitch)
{
    //! [GIVEN] A stream is open (playing, paused or recording)
    ON_CALL(*m_audioEngine, isBusy())
    .WillByDefault(Return(true));

    //! [THEN] Monitoring and the open stream are stopped before the device is switched
    InSequence seq;
    EXPECT_CALL(*m_audioEngine, stopMonitoring()).Times(1);
    EXPECT_CALL(*m_audioEngine, stopStream()).Times(1);
    EXPECT_CALL(*m_audioEngine, handleDeviceChange()).Times(1);

    //! [WHEN] The device is changed
    handleDeviceChange();
}

/**
 * @brief When nothing is playing there is no stream to stop.
 * @details Monitoring is still stopped (it may be active without making the
 *          engine "busy"), but stopStream() must not be called.
 */
TEST_F(Au3AudioDevicesProviderTests, HandleDeviceChange_WhenNotBusy_DoesNotStopStream)
{
    //! [GIVEN] No stream is open
    ON_CALL(*m_audioEngine, isBusy())
    .WillByDefault(Return(false));

    //! [THEN] Monitoring is stopped and the device is switched, but no stream is stopped
    EXPECT_CALL(*m_audioEngine, stopMonitoring()).Times(1);
    EXPECT_CALL(*m_audioEngine, stopStream()).Times(0);
    EXPECT_CALL(*m_audioEngine, handleDeviceChange()).Times(1);

    //! [WHEN] The device is changed
    handleDeviceChange();
}

/**
 * @brief Changing the capture channel count tears down an open stream.
 * @details The channel count can't change on an open stream, so setInputChannels()
 *          must go through handleDeviceChange() just like an input-device change.
 */
TEST_F(Au3AudioDevicesProviderTests, SetInputChannels_WhenBusy_TearsDownStream)
{
    //! [GIVEN] A stream is open (e.g. recording)
    ON_CALL(*m_audioEngine, isBusy())
    .WillByDefault(Return(true));

    //! [THEN] The open stream is stopped before the low-level switch
    InSequence seq;
    EXPECT_CALL(*m_audioEngine, stopMonitoring()).Times(1);
    EXPECT_CALL(*m_audioEngine, stopStream()).Times(1);
    EXPECT_CALL(*m_audioEngine, handleDeviceChange()).Times(1);

    //! [WHEN] The capture channel count is changed
    m_provider->setInputChannels(2);
}
}
