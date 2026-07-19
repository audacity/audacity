/*
 * Audacity: A Digital Audio Editor
 */
#include <cfloat>

#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "../internal/au3/au3record.h"

#include "actions/tests/mocks/actionsdispatchermock.h"
#include "context/tests/mocks/globalcontextmock.h"
#include "context/tests/mocks/playbackstatemock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "project/tests/testtools.h"
#include "trackedit/tests/mocks/selectioncontrollermock.h"
#include "trackedit/tests/mocks/trackeditprojectmock.h"

#include "mocks/audioenginemock.h"
#include "mocks/recordconfigurationmock.h"

#include "au3wrap/internal/au3project.h"
#include "au3wrap/au3types.h"

#include "au3-track/PendingTracks.h"
#include "au3-wave-track/WaveTrack.h"

using ::testing::_;
using ::testing::DoAll;
using ::testing::InSequence;
using ::testing::Invoke;
using ::testing::NiceMock;
using ::testing::Return;
using ::testing::SaveArg;
using ::testing::Truly;

using namespace au::au3;

namespace au::record {
constexpr static double TEST_SAMPLE_RATE = 44100.0;

class Au3RecordTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_record = std::make_shared<Au3Record>(muse::modularity::globalCtx());

        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_dispatcher = std::make_shared<NiceMock<muse::actions::ActionsDispatcherMock> >();
        m_selectionController = std::make_shared<NiceMock<trackedit::SelectionControllerMock> >();
        m_playbackState = std::make_shared<NiceMock<context::PlaybackStateMock> >();
        m_audioEngine = std::make_shared<NiceMock<audio::AudioEngineMock> >();
        m_recordConfiguration = std::make_shared<NiceMock<RecordConfigurationMock> >();
        m_trackeditProject = std::make_shared<NiceMock<trackedit::TrackeditProjectMock> >();
        m_currentProject = std::make_shared<NiceMock<project::AudacityProjectMock> >();

        m_record->globalContext.set(m_globalContext);
        m_record->dispatcher.set(m_dispatcher);
        m_record->selectionController.set(m_selectionController);
        m_record->audioEngine.set(m_audioEngine);
        m_record->recordConfiguration.set(m_recordConfiguration);

        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_currentProject));
        ON_CALL(*m_globalContext, playbackState())
        .WillByDefault(Return(m_playbackState));
        ON_CALL(*m_globalContext, currentTrackeditProject())
        .WillByDefault(Return(m_trackeditProject));

        ON_CALL(*m_audioEngine, isBusy())
        .WillByDefault(Invoke([this]() { return m_engineBusy; }));
        ON_CALL(*m_audioEngine, isCapturing())
        .WillByDefault(Return(false));
        ON_CALL(*m_audioEngine, stopStream())
        .WillByDefault(Invoke([this]() { m_engineBusy = false; }));
        ON_CALL(*m_audioEngine, recordingClipChanged())
        .WillByDefault(Return(m_recordingClipChanged));
        ON_CALL(*m_audioEngine, startStream(_, _, _, _, _, _, _, _, _))
        .WillByDefault(Return(1));

        ON_CALL(*m_recordConfiguration, leadInTimeDuration())
        .WillByDefault(Return(2.0));
        ON_CALL(*m_recordConfiguration, crossfadeDuration())
        .WillByDefault(Return(0.0));

        initTestProject();
    }

    void TearDown() override
    {
        PendingTracks::Get(projectRef()).ClearPendingTracks();
        Au3TrackList::Get(projectRef()).Clear();

        m_au3ProjectAccessor->clearSavedState();
        m_au3ProjectAccessor->close();

        testtools::removeProjectIfExists(m_workingProjectPath);
    }

    void initTestProject()
    {
        m_au3ProjectAccessor = std::make_shared<Au3ProjectAccessor>(muse::modularity::globalCtx());

        // Load a working copy of the empty-project fixture shared with the trackedit tests
        const std::string source = std::string(record_tests_DATA_ROOT) + "/../../trackedit/tests/data/empty.aup4";
        m_workingProjectPath = std::string(record_tests_DATA_ROOT) + "/empty_working.aup4";
        testtools::removeProjectIfExists(m_workingProjectPath);
        ASSERT_TRUE(testtools::copyFile(source, m_workingProjectPath));

        constexpr auto discardAutosave = false;
        muse::Ret ret = m_au3ProjectAccessor->load(muse::io::path_t(m_workingProjectPath), discardAutosave);
        ASSERT_TRUE(ret);

        ON_CALL(*m_currentProject, au3ProjectPtr())
        .WillByDefault(Return(m_au3ProjectAccessor->au3ProjectPtr()));
    }

    Au3Project& projectRef() const
    {
        return *reinterpret_cast<Au3Project*>(m_au3ProjectAccessor->au3ProjectPtr());
    }

    Au3WaveTrack* addSelectedMonoTrack()
    {
        auto& trackFactory = Au3WaveTrackFactory::Get(projectRef());
        auto track = trackFactory.Create(sampleFormat::floatSample, TEST_SAMPLE_RATE);
        Au3TrackList::Get(projectRef()).Add(track, ::TrackList::DoAssignId::Yes,
                                            ::TrackList::EventPublicationSynchrony::Synchronous);
        track->SetSelected(true);
        return track.get();
    }

    std::shared_ptr<Au3Record> m_record;

    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<muse::actions::ActionsDispatcherMock> m_dispatcher;
    std::shared_ptr<trackedit::SelectionControllerMock> m_selectionController;
    std::shared_ptr<context::PlaybackStateMock> m_playbackState;
    std::shared_ptr<audio::AudioEngineMock> m_audioEngine;
    std::shared_ptr<RecordConfigurationMock> m_recordConfiguration;
    std::shared_ptr<trackedit::TrackeditProjectMock> m_trackeditProject;
    std::shared_ptr<project::AudacityProjectMock> m_currentProject;

    std::shared_ptr<Au3ProjectAccessor> m_au3ProjectAccessor;
    std::string m_workingProjectPath;

    muse::async::Channel<Au3TrackId, Au3ClipId> m_recordingClipChanged;
    bool m_engineBusy = false;
};

TEST_F(Au3RecordTests, LeadInRecordingStartsAtPlayheadPosition)
{
    //! [GIVEN] A selected track, the playhead at 8s and a stale selection start at 5s
    //! (e.g. left behind by a previous recording)
    addSelectedMonoTrack();
    ON_CALL(*m_playbackState, playbackPosition())
    .WillByDefault(Return(muse::secs_t(8.0)));
    ON_CALL(*m_selectionController, selectionStartTime())
    .WillByDefault(Return(trackedit::secs_t(5.0)));

    double startedAt = -1.0;
    double leadInTime = -1.0;
    EXPECT_CALL(*m_audioEngine, startStream(_, _, _, _, _, _, _, _, _))
    .WillOnce(DoAll(SaveArg<1>(&startedAt), SaveArg<7>(&leadInTime), Return(1)));

    //! [THEN] The playhead is repositioned to the start of the lead-in (8s - 2s pre-roll)
    EXPECT_CALL(*m_dispatcher,
                dispatch(testing::Matcher<const muse::actions::ActionQuery&>(
                             Truly([](const muse::actions::ActionQuery& q) {
        return q.param("seekTime").toDouble() == 6.0;
    }))));

    //! [WHEN] Lead-in recording is initiated
    muse::Ret ret = m_record->leadInRecording();
    EXPECT_TRUE(ret);

    //! [THEN] The stream starts recording at the playhead position, not at the
    //! stale selection start
    EXPECT_DOUBLE_EQ(startedAt, 8.0);
    EXPECT_DOUBLE_EQ(leadInTime, 2.0);
}

TEST_F(Au3RecordTests, LeadInRecordingAtZeroPlayheadFails)
{
    //! [GIVEN] A selected track and the playhead at 0s — there is nothing to pre-roll
    addSelectedMonoTrack();
    ON_CALL(*m_playbackState, playbackPosition())
    .WillByDefault(Return(muse::secs_t(0.0)));

    EXPECT_CALL(*m_audioEngine, startStream(_, _, _, _, _, _, _, _, _))
    .Times(0);

    //! [WHEN] Lead-in recording is initiated
    muse::Ret ret = m_record->leadInRecording();

    //! [THEN] It fails and no stream is started
    EXPECT_FALSE(ret);
}

TEST_F(Au3RecordTests, RecordingWithBusyEngineStartsUnpausedAtPlayhead)
{
    //! [GIVEN] A selected track, the playhead at 8s and the engine still holding
    //! a (possibly paused) playback stream
    addSelectedMonoTrack();
    m_engineBusy = true;
    ON_CALL(*m_playbackState, playbackPosition())
    .WillByDefault(Return(muse::secs_t(8.0)));

    double startedAt = -1.0;

    //! [THEN] The old stream is stopped, the engine pause flag (which survives
    //! stream restarts) is cleared, and only then recording starts
    InSequence seq;
    EXPECT_CALL(*m_audioEngine, stopStream());
    EXPECT_CALL(*m_audioEngine, pauseStream(false));
    EXPECT_CALL(*m_audioEngine, startStream(_, _, _, _, _, _, _, _, _))
    .WillOnce(DoAll(SaveArg<1>(&startedAt), Return(1)));

    //! [WHEN] Recording is initiated
    muse::Ret ret = m_record->start();
    EXPECT_TRUE(ret);

    //! [THEN] Recording starts at the playhead position
    EXPECT_DOUBLE_EQ(startedAt, 8.0);
}
}
