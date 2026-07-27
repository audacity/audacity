/*
 * Audacity: A Digital Audio Editor
 */
#include <stdexcept>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include "context/tests/mocks/globalcontextmock.h"
#include "global/tests/mocks/applicationmock.h"
#include "project/tests/mocks/audacityprojectmock.h"

#include "audio/iaudiostreamsuspender.h"
#include "mocks/audioenginemock.h"

#include "../internal/au3audiodrivercontroller.h"

using ::testing::NiceMock;
using ::testing::Return;

namespace au::au3audio {
namespace {
struct SuspensionState {
    int suspendCalls = 0;
    int restoreCalls = 0;
    bool restoreResult = true;
    audio::AudioStreamKind lastKind = audio::AudioStreamKind::Playback;
};

class TestSuspender final : public audio::IAudioStreamSuspender
{
public:
    explicit TestSuspender(SuspensionState& state)
        : m_state(state) {}

    audio::AudioStreamRestorer suspendForAudioConfiguration(audio::AudioStreamKind streamKind) override
    {
        ++m_state.suspendCalls;
        m_state.lastKind = streamKind;
        if (throwOnSuspend) {
            throw std::runtime_error("suspend failed");
        }
        return [this] {
            ++m_state.restoreCalls;
            return m_state.restoreResult;
        };
    }

    bool throwOnSuspend = false;

private:
    SuspensionState& m_state;
};
}

class Au3AudioDriverControllerTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_ownerContext = std::make_shared<muse::modularity::Context>(101);
        m_requesterContext = std::make_shared<muse::modularity::Context>(102);

        m_ownerGlobalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_requesterGlobalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_ownerProject = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        m_requesterProject = std::make_shared<NiceMock<project::AudacityProjectMock> >();

        ON_CALL(*m_ownerProject, au3ProjectPtr())
        .WillByDefault(Return(reinterpret_cast<uintptr_t>(&m_ownerProjectStorage)));
        ON_CALL(*m_requesterProject, au3ProjectPtr())
        .WillByDefault(Return(reinterpret_cast<uintptr_t>(&m_requesterProjectStorage)));
        ON_CALL(*m_ownerGlobalContext, currentProject()).WillByDefault(Return(m_ownerProject));
        ON_CALL(*m_requesterGlobalContext, currentProject()).WillByDefault(Return(m_requesterProject));

        m_ownerSuspender = std::make_shared<TestSuspender>(m_ownerSuspension);
        m_requesterSuspender = std::make_shared<TestSuspender>(m_requesterSuspension);

        muse::modularity::ioc(m_ownerContext)->registerExport<context::IGlobalContext>("utests", m_ownerGlobalContext);
        muse::modularity::ioc(m_ownerContext)->registerExport<audio::IAudioStreamSuspender>("utests", m_ownerSuspender);
        muse::modularity::ioc(m_requesterContext)->registerExport<context::IGlobalContext>("utests", m_requesterGlobalContext);
        muse::modularity::ioc(m_requesterContext)->registerExport<audio::IAudioStreamSuspender>("utests", m_requesterSuspender);

        m_application = std::make_shared<NiceMock<muse::ApplicationMock> >();
        ON_CALL(*m_application, contexts())
        .WillByDefault(Return(std::vector<muse::modularity::ContextPtr> { m_requesterContext, m_ownerContext }));
        m_controller.application.set(m_application);

        m_audioEngine = std::make_shared<NiceMock<audio::AudioEngineMock> >();
        m_controller.audioEngine.set(m_audioEngine);

        m_controller.m_configuration.bufferLength = 100.0;
        m_controller.m_configuration.defaultSampleRate = 44100;
        m_controller.m_configuration.asioUseDeviceSampleRate = true;
    }

    void TearDown() override
    {
        muse::modularity::removeIoC(m_ownerContext);
        muse::modularity::removeIoC(m_requesterContext);
    }

    audio::AudioStreamDescriptor stream(audio::AudioStreamKind kind = audio::AudioStreamKind::Playback)
    {
        return { kind, reinterpret_cast<AudacityProject*>(&m_ownerProjectStorage), 44100.0 };
    }

    void setApplying(bool applying)
    {
        m_controller.m_applying = applying;
    }

    Au3AudioDriverController m_controller;
    std::shared_ptr<NiceMock<audio::AudioEngineMock> > m_audioEngine;
    std::shared_ptr<NiceMock<muse::ApplicationMock> > m_application;
    muse::modularity::ContextPtr m_ownerContext;
    muse::modularity::ContextPtr m_requesterContext;
    std::shared_ptr<NiceMock<context::GlobalContextMock> > m_ownerGlobalContext;
    std::shared_ptr<NiceMock<context::GlobalContextMock> > m_requesterGlobalContext;
    std::shared_ptr<NiceMock<project::AudacityProjectMock> > m_ownerProject;
    std::shared_ptr<NiceMock<project::AudacityProjectMock> > m_requesterProject;
    std::shared_ptr<TestSuspender> m_ownerSuspender;
    std::shared_ptr<TestSuspender> m_requesterSuspender;
    SuspensionState m_ownerSuspension;
    SuspensionState m_requesterSuspension;
    int m_ownerProjectStorage = 0;
    int m_requesterProjectStorage = 0;
};

TEST_F(Au3AudioDriverControllerTests, Apply_DerivesStreamOwnerInsteadOfTrustingRequester)
{
    ON_CALL(*m_audioEngine, currentStream()).WillByDefault(Return(stream()));
    audio::AudioConfigurationChange change;
    change.bufferLength = 50.0;

    const auto result = m_controller.apply(m_requesterContext, change);

    EXPECT_EQ(result.status, audio::ApplyStatus::Applied);
    EXPECT_EQ(m_ownerSuspension.suspendCalls, 1);
    EXPECT_EQ(m_ownerSuspension.restoreCalls, 1);
    EXPECT_EQ(m_requesterSuspension.suspendCalls, 0);
}

TEST_F(Au3AudioDriverControllerTests, Apply_MultipleFieldsUseOneSuspension)
{
    ON_CALL(*m_audioEngine, currentStream()).WillByDefault(Return(stream()));
    audio::AudioConfigurationChange change;
    change.bufferLength = 50.0;
    change.asioUseDeviceSampleRate = false;

    const auto result = m_controller.apply(m_requesterContext, change);

    EXPECT_TRUE(result.succeeded());
    EXPECT_EQ(m_ownerSuspension.suspendCalls, 1);
    EXPECT_EQ(m_ownerSuspension.restoreCalls, 1);
}

TEST_F(Au3AudioDriverControllerTests, Apply_CustomSampleRateIsValidAndDoesNotReopenPlayback)
{
    ON_CALL(*m_audioEngine, currentStream()).WillByDefault(Return(stream()));
    audio::AudioConfigurationChange change;
    change.defaultSampleRate = 12345;

    const auto result = m_controller.apply({}, change);

    EXPECT_EQ(result.status, audio::ApplyStatus::Applied);
    EXPECT_EQ(m_controller.configuration().defaultSampleRate, 12345u);
    EXPECT_EQ(m_ownerSuspension.suspendCalls, 0);
}

TEST_F(Au3AudioDriverControllerTests, Apply_RequesterScopedChangeDoesNotSuspendAnotherContextsStream)
{
    ON_CALL(*m_audioEngine, currentStream())
    .WillByDefault(Return(stream(audio::AudioStreamKind::Recording)));
    audio::AudioConfigurationChange change;
    change.latencyCompensation = 25.0;

    const auto result = m_controller.apply(m_requesterContext, change);

    EXPECT_EQ(result.status, audio::ApplyStatus::Applied);
    EXPECT_EQ(m_ownerSuspension.suspendCalls, 0);
}

TEST_F(Au3AudioDriverControllerTests, Apply_OrphanStreamIsForceStoppedInsteadOfWedgingSettings)
{
    auto orphan = stream();
    orphan.ownerProject = reinterpret_cast<AudacityProject*>(0x1234);
    EXPECT_CALL(*m_audioEngine, currentStream())
    .WillOnce(Return(orphan))
    .WillRepeatedly(Return(std::optional<audio::AudioStreamDescriptor>()));
    EXPECT_CALL(*m_audioEngine, stopStream()).Times(1);
    audio::AudioConfigurationChange change;
    change.bufferLength = 50.0;

    const auto result = m_controller.apply(m_requesterContext, change);

    EXPECT_EQ(result.status, audio::ApplyStatus::Applied);
    EXPECT_EQ(m_controller.configuration().bufferLength, 50.0);
    EXPECT_EQ(m_ownerSuspension.suspendCalls, 0);
}

TEST_F(Au3AudioDriverControllerTests, Apply_OrphanStreamThatSurvivesStopFailsBeforeWritingSettings)
{
    auto orphan = stream();
    orphan.ownerProject = reinterpret_cast<AudacityProject*>(0x1234);
    ON_CALL(*m_audioEngine, currentStream()).WillByDefault(Return(orphan));
    audio::AudioConfigurationChange change;
    change.bufferLength = 50.0;

    const auto result = m_controller.apply(m_requesterContext, change);

    EXPECT_EQ(result.status, audio::ApplyStatus::OwnerUnavailable);
    EXPECT_EQ(m_controller.configuration().bufferLength, 100.0);
}

TEST_F(Au3AudioDriverControllerTests, Apply_RestoreFailureLeavesAppliedConfigurationAndReportsIt)
{
    m_ownerSuspension.restoreResult = false;
    ON_CALL(*m_audioEngine, currentStream()).WillByDefault(Return(stream()));
    audio::AudioConfigurationChange change;
    change.bufferLength = 50.0;

    const auto result = m_controller.apply(m_requesterContext, change);

    EXPECT_EQ(result.status, audio::ApplyStatus::Applied);
    EXPECT_TRUE(result.succeeded());
    EXPECT_TRUE(result.streamRestorationFailed);
    EXPECT_EQ(m_controller.configuration().bufferLength, 50.0);
}

TEST_F(Au3AudioDriverControllerTests, Apply_RecordingIsSuspendedButNeverRestoredAsRecording)
{
    ON_CALL(*m_audioEngine, currentStream())
    .WillByDefault(Return(stream(audio::AudioStreamKind::Recording)));
    audio::AudioConfigurationChange change;
    change.bufferLength = 50.0;

    const auto result = m_controller.apply(m_requesterContext, change);

    EXPECT_EQ(result.status, audio::ApplyStatus::Applied);
    EXPECT_TRUE(result.succeeded());
    EXPECT_EQ(m_ownerSuspension.lastKind, audio::AudioStreamKind::Recording);
}

TEST_F(Au3AudioDriverControllerTests, Apply_SuspensionExceptionIsContainedAndDoesNotChangeConfiguration)
{
    m_ownerSuspender->throwOnSuspend = true;
    ON_CALL(*m_audioEngine, currentStream()).WillByDefault(Return(stream()));
    audio::AudioConfigurationChange change;
    change.bufferLength = 50.0;

    const auto result = m_controller.apply(m_requesterContext, change);

    EXPECT_EQ(result.status, audio::ApplyStatus::InternalError);
    EXPECT_EQ(m_controller.configuration().bufferLength, 100.0);
}

TEST_F(Au3AudioDriverControllerTests, Apply_ReentrantRequestIsRejectedWithoutChangingConfiguration)
{
    setApplying(true);
    audio::AudioConfigurationChange change;
    change.bufferLength = 50.0;

    const auto result = m_controller.apply(m_requesterContext, change);

    EXPECT_EQ(result.status, audio::ApplyStatus::Busy);
    EXPECT_EQ(m_controller.configuration().bufferLength, 100.0);
}
}
