/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "context/tests/mocks/globalcontextmock.h"
#include "mocks/audioenginemock.h"
#include "project/tests/mocks/audacityprojectmock.h"

#include "../internal/au3audiodevicesprovider.h"

using ::testing::_;
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

        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_provider->globalContext.set(m_globalContext);

        m_currentProject = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_currentProject));

        // The provider reaches the au3 project through au3ProjectPtr(); hand it
        // a (never dereferenced) non-null dummy so the null-guard doesn't skip
        // the code under test.
        static int dummyAu3Project;
        ON_CALL(*m_currentProject, au3ProjectPtr())
        .WillByDefault(Return(reinterpret_cast<uintptr_t>(&dummyAu3Project)));
    }

    void handleDeviceChange()
    {
        m_provider->handleDeviceChange();
    }

    //! withMonitoringRestored() is private; the fixture is a friend of the
    //! provider, so this thin wrapper is how the tests reach it.
    void withMonitoringRestored(const std::function<void()>& change)
    {
        m_provider->withMonitoringRestored(change);
    }

    std::shared_ptr<Au3AudioDevicesProvider> m_provider;
    std::shared_ptr<audio::AudioEngineMock> m_audioEngine;
    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<project::AudacityProjectMock> m_currentProject;
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

    //! [GIVEN] A channel count different from the one currently selected (settings
    //! persist across test runs via muse::settings(), so this must not assume a
    //! fresh/default value)
    const int newCount = m_provider->inputChannelsSelected() + 1;

    //! [THEN] The open stream is stopped before the low-level switch
    InSequence seq;
    EXPECT_CALL(*m_audioEngine, stopMonitoring()).Times(1);
    EXPECT_CALL(*m_audioEngine, stopStream()).Times(1);
    EXPECT_CALL(*m_audioEngine, handleDeviceChange()).Times(1);

    //! [WHEN] The capture channel count is changed
    m_provider->setInputChannels(newCount);
}

/**
 * @brief Input monitoring must survive a device action.
 * @details handleDeviceChange() unconditionally stops monitoring to perform the
 *          low-level switch (issue #11098); if it was on beforehand, it must be
 *          restarted afterwards, or the user silently loses input metering.
 */
TEST_F(Au3AudioDevicesProviderTests, HandleDeviceChange_WhenMonitoring_RestartsMonitoringAfterSwitch)
{
    //! [GIVEN] Input monitoring is on
    ON_CALL(*m_audioEngine, isMonitoring())
    .WillByDefault(Return(true));

    //! [THEN] Monitoring is stopped for the switch and restarted once it's done
    InSequence seq;
    EXPECT_CALL(*m_audioEngine, stopMonitoring()).Times(1);
    EXPECT_CALL(*m_audioEngine, handleDeviceChange()).Times(1);
    EXPECT_CALL(*m_audioEngine, startMonitoring(_)).Times(1);

    //! [WHEN] The device is changed
    handleDeviceChange();
}

/**
 * @brief No spurious monitoring start when it wasn't on to begin with.
 */
TEST_F(Au3AudioDevicesProviderTests, HandleDeviceChange_WhenNotMonitoring_DoesNotRestartMonitoring)
{
    //! [GIVEN] Input monitoring is off
    ON_CALL(*m_audioEngine, isMonitoring())
    .WillByDefault(Return(false));

    //! [THEN] Nothing tries to resume monitoring
    EXPECT_CALL(*m_audioEngine, startMonitoring(_)).Times(0);

    //! [WHEN] The device is changed
    handleDeviceChange();
}

/**
 * @brief A composite change restores monitoring once, at the outermost scope.
 * @details A host switch cascades into an output- and an input-device change,
 *          each funneling through handleDeviceChange(). Monitoring must not be
 *          restarted on the half-switched configuration in between — only once,
 *          after the whole composite change is done.
 */
TEST_F(Au3AudioDevicesProviderTests, WithMonitoringRestored_NestedDeviceChanges_RestoresMonitoringOnce)
{
    //! [GIVEN] Input monitoring is on
    ON_CALL(*m_audioEngine, isMonitoring())
    .WillByDefault(Return(true));

    //! [THEN] Both low-level switches run before monitoring is restored, and it
    //! is restored exactly once
    InSequence seq;
    EXPECT_CALL(*m_audioEngine, handleDeviceChange()).Times(2);
    EXPECT_CALL(*m_audioEngine, startMonitoring(_)).Times(1);

    //! [WHEN] A composite change funnels through handleDeviceChange() twice
    withMonitoringRestored([this]() {
        handleDeviceChange();
        handleDeviceChange();
    });
}

/**
 * @brief A buffer-length change must reach an active monitoring stream.
 * @details The buffer length is consumed when a stream is opened; an already
 *          running input-monitoring stream keeps the old value, so it has to
 *          be reopened around the change for the setting to take effect.
 */
TEST_F(Au3AudioDevicesProviderTests, SetBufferLength_WhenMonitoring_RestartsMonitoring)
{
    //! [GIVEN] Input monitoring is on
    ON_CALL(*m_audioEngine, isMonitoring())
    .WillByDefault(Return(true));

    //! [THEN] Monitoring is stopped, then restarted once the value is applied
    InSequence seq;
    EXPECT_CALL(*m_audioEngine, stopMonitoring()).Times(1);
    EXPECT_CALL(*m_audioEngine, startMonitoring(_)).Times(1);

    //! [WHEN] The buffer length is changed
    m_provider->setBufferLength(50.0);
}

/**
 * @brief A default-sample-rate change must reach an active monitoring stream.
 */
TEST_F(Au3AudioDevicesProviderTests, SetDefaultSampleRate_WhenMonitoring_RestartsMonitoring)
{
    //! [GIVEN] Input monitoring is on
    ON_CALL(*m_audioEngine, isMonitoring())
    .WillByDefault(Return(true));

    //! [GIVEN] A rate different from the current one (settings persist across
    //! test runs via muse::settings(), so this must not assume a fresh value)
    const uint64_t newRate = m_provider->defaultSampleRate() == 48000 ? 44100 : 48000;

    //! [THEN] Monitoring is stopped, then restarted once the value is applied
    InSequence seq;
    EXPECT_CALL(*m_audioEngine, stopMonitoring()).Times(1);
    EXPECT_CALL(*m_audioEngine, startMonitoring(_)).Times(1);

    //! [WHEN] The default sample rate is changed
    m_provider->setDefaultSampleRate(newRate);
}

/**
 * @brief Re-selecting the current sample rate must not disturb monitoring.
 */
TEST_F(Au3AudioDevicesProviderTests, SetDefaultSampleRate_SameValueAsCurrent_DoesNotTouchMonitoring)
{
    //! [GIVEN] Input monitoring is on
    ON_CALL(*m_audioEngine, isMonitoring())
    .WillByDefault(Return(true));

    //! [THEN] The no-op selection leaves the monitoring stream alone
    EXPECT_CALL(*m_audioEngine, stopMonitoring()).Times(0);
    EXPECT_CALL(*m_audioEngine, startMonitoring(_)).Times(0);

    //! [WHEN] The already-current rate is selected again
    m_provider->setDefaultSampleRate(m_provider->defaultSampleRate());
}

/**
 * @brief No spurious monitoring start when it wasn't on to begin with.
 */
TEST_F(Au3AudioDevicesProviderTests, SetBufferLength_WhenNotMonitoring_DoesNotStartMonitoring)
{
    //! [GIVEN] Input monitoring is off
    ON_CALL(*m_audioEngine, isMonitoring())
    .WillByDefault(Return(false));

    //! [THEN] Nothing tries to start monitoring
    EXPECT_CALL(*m_audioEngine, startMonitoring(_)).Times(0);

    //! [WHEN] The buffer length is changed
    m_provider->setBufferLength(50.0);
}

TEST_F(Au3AudioDevicesProviderTests, SetInputChannels_SameValueAsCurrent_DoesNotTearDownStream)
{
    //! [GIVEN] Whatever channel count is currently selected (settings persist
    //! across test runs via muse::settings(), so this must not assume a
    //! fresh/default value)
    const int currentCount = m_provider->inputChannelsSelected();

    //! [THEN] Re-selecting the same count must not touch the stream at all
    EXPECT_CALL(*m_audioEngine, stopMonitoring()).Times(0);
    EXPECT_CALL(*m_audioEngine, stopStream()).Times(0);
    EXPECT_CALL(*m_audioEngine, handleDeviceChange()).Times(0);

    //! [WHEN] The same channel count is selected again (a no-op selection)
    m_provider->setInputChannels(currentCount);
}
}
