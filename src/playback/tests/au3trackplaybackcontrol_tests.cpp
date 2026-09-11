/*
 * Audacity: A Digital Audio Editor
 */
#include <map>

#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "../internal/au3/au3trackplaybackcontrol.h"

#include "context/tests/mocks/globalcontextmock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "project/tests/testtools.h"
#include "trackedit/tests/mocks/projecthistorymock.h"

#include "au3wrap/internal/au3project.h"
#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/au3types.h"

#include "au3-label-track/LabelTrack.h"
#include "au3-wave-track/WaveTrack.h"

using ::testing::An;
using ::testing::NiceMock;
using ::testing::Return;

using namespace au::au3;

namespace au::playback {
class Au3TrackPlaybackControlTests : public ::testing::Test, public muse::async::Asyncable
{
public:
    void SetUp() override
    {
        m_control = std::make_shared<Au3TrackPlaybackControl>(muse::modularity::globalCtx());

        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_control->globalContext.set(m_globalContext);

        m_projectHistory = std::make_shared<NiceMock<trackedit::ProjectHistoryMock> >();
        m_control->projectHistory.set(m_projectHistory);

        m_currentProject = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_currentProject));

        initTestProject();

        m_control->muteOrSoloChanged().onReceive(this, [this](long trackId) {
            ++m_notificationCount[trackId];
        });
    }

    void TearDown() override
    {
        // Guard against a fatal failure during setup leaving the project uninitialized
        if (m_au3ProjectAccessor && m_au3ProjectAccessor->au3ProjectPtr()) {
            Au3TrackList::Get(projectRef()).Clear();

            m_au3ProjectAccessor->clearSavedState();
            m_au3ProjectAccessor->close();
        }

        testtools::removeProjectIfExists(m_workingProjectPath);
    }

    void initTestProject()
    {
        m_au3ProjectAccessor = std::make_shared<Au3ProjectAccessor>(muse::modularity::globalCtx());

        // Load a working copy of the empty-project fixture shared with the trackedit tests
        const std::string source = std::string(playback_tests_DATA_ROOT) + "/../../trackedit/tests/data/empty.aup4";
        m_workingProjectPath = std::string(playback_tests_DATA_ROOT) + "/empty_working.aup4";
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

    enum class Muted {
        Yes,
        No,
    };

    trackedit::TrackId addWaveTrack(Muted muted)
    {
        auto& trackFactory = Au3WaveTrackFactory::Get(projectRef());
        auto track = trackFactory.Create(sampleFormat::floatSample, 44100.0);
        Au3TrackList::Get(projectRef()).Add(track, ::TrackList::DoAssignId::Yes,
                                            ::TrackList::EventPublicationSynchrony::Synchronous);
        track->SetMute(muted == Muted::Yes);
        return track->GetId();
    }

    trackedit::TrackId addLabelTrack()
    {
        auto& trackList = Au3TrackList::Get(projectRef());
        auto track = ::LabelTrack::CreatePtr(trackList);
        trackList.Add(track);
        return track->GetId();
    }

    void expectHistoryModifiedTimes(int times)
    {
        EXPECT_CALL(*m_projectHistory, modifyState(An<bool>())).Times(times);
        EXPECT_CALL(*m_projectHistory, markUnsaved()).Times(times);
    }

    size_t totalNotificationCount() const
    {
        size_t total = 0;
        for (const auto& [trackId, count] : m_notificationCount) {
            total += count;
        }
        return total;
    }

    std::shared_ptr<Au3TrackPlaybackControl> m_control;

    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<trackedit::ProjectHistoryMock> m_projectHistory;
    std::shared_ptr<project::AudacityProjectMock> m_currentProject;

    std::shared_ptr<Au3ProjectAccessor> m_au3ProjectAccessor;
    std::string m_workingProjectPath;

    std::map<long, int> m_notificationCount;
};

TEST_F(Au3TrackPlaybackControlTests, MuteAllMutesEveryTrack)
{
    //! [GIVEN] Three unmuted tracks
    const trackedit::TrackIdList trackIds { addWaveTrack(Muted::No), addWaveTrack(Muted::No), addWaveTrack(Muted::No) };

    //! [WHEN] Muting them all at once
    m_control->setMuted(trackIds, true);

    //! [THEN] Every one of them is muted
    for (const trackedit::TrackId trackId : trackIds) {
        EXPECT_TRUE(m_control->muted(trackId)) << "Track " << trackId << " should be muted";
    }
}

TEST_F(Au3TrackPlaybackControlTests, UnmuteAllUnmutesEveryTrack)
{
    //! [GIVEN] Three muted tracks
    const trackedit::TrackIdList trackIds { addWaveTrack(Muted::Yes), addWaveTrack(Muted::Yes), addWaveTrack(Muted::Yes) };

    //! [WHEN] Unmuting them all at once
    m_control->setMuted(trackIds, false);

    //! [THEN] Every one of them is unmuted
    for (const trackedit::TrackId trackId : trackIds) {
        EXPECT_FALSE(m_control->muted(trackId)) << "Track " << trackId << " should be unmuted";
    }
}

TEST_F(Au3TrackPlaybackControlTests, MuteAllNotifiesEachTrackOnceAndModifiesHistoryOnce)
{
    //! [GIVEN] Three unmuted tracks
    const trackedit::TrackIdList trackIds { addWaveTrack(Muted::No), addWaveTrack(Muted::No), addWaveTrack(Muted::No) };

    //! [THEN] The project history is modified exactly once
    expectHistoryModifiedTimes(1);

    //! [WHEN] Muting them all at once
    m_control->setMuted(trackIds, true);

    //! [THEN] Each track is notified exactly once
    EXPECT_EQ(totalNotificationCount(), trackIds.size());
    for (const trackedit::TrackId trackId : trackIds) {
        EXPECT_EQ(m_notificationCount[trackId], 1) << "Track " << trackId << " should be notified exactly once";
    }
}

TEST_F(Au3TrackPlaybackControlTests, UnmuteAllNotifiesEachTrackOnceAndModifiesHistoryOnce)
{
    //! [GIVEN] Three muted tracks
    const trackedit::TrackIdList trackIds { addWaveTrack(Muted::Yes), addWaveTrack(Muted::Yes), addWaveTrack(Muted::Yes) };

    //! [THEN] The project history is modified exactly once
    expectHistoryModifiedTimes(1);

    //! [WHEN] Unmuting them all at once
    m_control->setMuted(trackIds, false);

    //! [THEN] Each track is notified exactly once
    EXPECT_EQ(totalNotificationCount(), trackIds.size());
    for (const trackedit::TrackId trackId : trackIds) {
        EXPECT_EQ(m_notificationCount[trackId], 1) << "Track " << trackId << " should be notified exactly once";
    }
}

TEST_F(Au3TrackPlaybackControlTests, MuteAllWithSomeTracksAlreadyMutedIsStillOneOperation)
{
    //! [GIVEN] Three tracks, the middle one already muted
    const trackedit::TrackIdList trackIds { addWaveTrack(Muted::No), addWaveTrack(Muted::Yes), addWaveTrack(Muted::No) };

    //! [THEN] The project history is still modified exactly once
    expectHistoryModifiedTimes(1);

    //! [WHEN] Muting them all at once
    m_control->setMuted(trackIds, true);

    //! [THEN] All tracks are muted, each notified exactly once
    EXPECT_EQ(totalNotificationCount(), trackIds.size());
    for (const trackedit::TrackId trackId : trackIds) {
        EXPECT_TRUE(m_control->muted(trackId)) << "Track " << trackId << " should be muted";
        EXPECT_EQ(m_notificationCount[trackId], 1) << "Track " << trackId << " should be notified exactly once";
    }
}

TEST_F(Au3TrackPlaybackControlTests, MuteAllWhenAllTracksAlreadyMutedDoesNothing)
{
    //! [GIVEN] Three tracks, all already muted
    const trackedit::TrackIdList trackIds { addWaveTrack(Muted::Yes), addWaveTrack(Muted::Yes), addWaveTrack(Muted::Yes) };

    //! [THEN] The project history is not touched
    expectHistoryModifiedTimes(0);

    //! [WHEN] Muting them all at once
    m_control->setMuted(trackIds, true);

    //! [THEN] Nobody is notified
    EXPECT_EQ(totalNotificationCount(), 0u);
}

TEST_F(Au3TrackPlaybackControlTests, MuteAllWithOnlyNonWaveTracksDoesNothing)
{
    //! [GIVEN] A label track only
    const trackedit::TrackIdList trackIds { addLabelTrack() };

    //! [THEN] The project history is not touched
    expectHistoryModifiedTimes(0);

    //! [WHEN] Muting it
    m_control->setMuted(trackIds, true);

    //! [THEN] Nobody is notified
    EXPECT_EQ(totalNotificationCount(), 0u);
}
}
