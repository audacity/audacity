/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "../internal/au3/au3changedetection.h"

#include "context/tests/mocks/globalcontextmock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "mocks/trackeditprojectmock.h"

#include "au3wrap/internal/au3project.h"
#include "au3wrap/internal/domaccessor.h"

using ::testing::NiceMock;
using ::testing::Return;
using ::testing::Truly;
using ::testing::_;

using namespace au;
using namespace au::au3;

namespace au::trackedit {
class Au3ChangeDetectionTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_au3ChangeDetection = std::make_shared<Au3ChangeDetection>();

        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_au3ChangeDetection->globalContext.set(m_globalContext);

        m_trackEditProject = std::make_shared<NiceMock<TrackeditProjectMock> >();
        ON_CALL(*m_globalContext, currentTrackeditProject())
        .WillByDefault(Return(m_trackEditProject));

        m_currentProject = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_currentProject));
    }

    void TearDown() override
    {
    }

    static TracksAndClips buildTracksAndClips()
    {
        constexpr int trackCount = 5;

        std::pair<TrackList, std::vector<trackedit::Clips> > structure;

        // Adding tracks and clips vectors:
        for (int i = 0; i < trackCount; ++i) {
            addOneTrack(structure, i);
        }

        // Adding an increasing number of clips to each track:
        for (int i = 0; i < trackCount; ++i) {
            for (int j = 0; j <= i; ++j) {
                addClipToTrack(structure, i, j);
            }
        }

        return structure;
    }

    static void addOneTrack(TracksAndClips& structure, int id)
    {
        structure.first.emplace_back();
        structure.second.emplace_back();
        structure.first.back().id = id;

        // None of the below should cause a change trigger:
        structure.first.back().title = std::to_string(id).c_str();

        switch (std::rand() % 4) {
        case 1:
            structure.first.back().type = TrackType::Mono;
            break;
        case 2:
            structure.first.back().type = TrackType::Stereo;
            break;
        case 3:
            structure.first.back().type = TrackType::Label;
            break;
        default:
            structure.first.back().type = TrackType::Undefined;
        }

        structure.first.back().color = muse::draw::Color(std::rand() % 256, std::rand() % 256, std::rand() % 256);
    }

    static void addClipToTrack(TracksAndClips& structure, int trackId, int j)
    {
        auto& newClip = structure.second[trackId].emplace_back();
        newClip.key.clipId = j;
        newClip.key.trackId = trackId;

        // None of the below should cause a change trigger:
        newClip.title = std::to_string(j).c_str();

        if (j % 2 == 0) {
            newClip.color = muse::draw::Color(std::rand() % 256, std::rand() % 256, std::rand() % 256);
            newClip.hasCustomColor = true;
            newClip.optimizeForVoice = false;
            newClip.stretchToMatchTempo = true;
        } else {
            newClip.hasCustomColor = false;
            newClip.optimizeForVoice = true;
            newClip.stretchToMatchTempo = false;
        }
    }

    // The class tested.
    std::shared_ptr<Au3ChangeDetection> m_au3ChangeDetection;

    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<project::AudacityProjectMock> m_currentProject;
    std::shared_ptr<TrackeditProjectMock> m_trackEditProject;
};

TEST_F(Au3ChangeDetectionTests, TestNotificationsWhenTheresNoChanges)
{
    TracksAndClips before = buildTracksAndClips();
    TracksAndClips after = buildTracksAndClips();

    //! The above are equal. No change notifications are expected.

    EXPECT_EQ(before.first.size(), after.first.size());

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    m_au3ChangeDetection->notifyOfUndoRedo(before, after);
}

////////////////////////////////////////////////
/// Tests for track change notification:
////////////////////////////////////////////////

TEST_F(Au3ChangeDetectionTests, TestTrackNotificationsForAddingOneTrack)
{
    TracksAndClips before = buildTracksAndClips();
    TracksAndClips after = buildTracksAndClips();

    addOneTrack(after, after.first.size());

    EXPECT_NE(before.first.size(), after.first.size());

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(1);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    m_au3ChangeDetection->notifyOfUndoRedo(before, after);
}

TEST_F(Au3ChangeDetectionTests, TestTrackNotificationsForAddingTwoTracks)
{
    TracksAndClips before = buildTracksAndClips();
    TracksAndClips after = buildTracksAndClips();

    addOneTrack(after, after.first.size());
    addOneTrack(after, after.first.size());

    EXPECT_NE(before.first.size(), after.first.size());

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(2);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    m_au3ChangeDetection->notifyOfUndoRedo(before, after);
}

TEST_F(Au3ChangeDetectionTests, TestTrackNotificationsForRemovingOneTrack)
{
    TracksAndClips before = buildTracksAndClips();
    TracksAndClips after = buildTracksAndClips();

    after.first.pop_back();
    after.second.pop_back();

    EXPECT_NE(before.first.size(), after.first.size());

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(1);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    m_au3ChangeDetection->notifyOfUndoRedo(before, after);
}

TEST_F(Au3ChangeDetectionTests, TestTrackNotificationsForRemovingTwoTracks)
{
    TracksAndClips before = buildTracksAndClips();
    TracksAndClips after = buildTracksAndClips();

    after.first.pop_back();
    after.second.pop_back();

    after.first.pop_back();
    after.second.pop_back();

    EXPECT_NE(before.first.size(), after.first.size());

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(2);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    m_au3ChangeDetection->notifyOfUndoRedo(before, after);
}

TEST_F(Au3ChangeDetectionTests, TestTrackNotificationsForReordering)
{
    TracksAndClips before = buildTracksAndClips();
    TracksAndClips after = buildTracksAndClips();

    EXPECT_EQ(before.first.size(), after.first.size());

    // Causes reordering detection:
    before.first.back().id = 0;
    before.first.front().id = 5;

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(1);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    m_au3ChangeDetection->notifyOfUndoRedo(before, after);
}

////////////////////////////////////////////////
/// Tests for clip change notification:
////////////////////////////////////////////////

TEST_F(Au3ChangeDetectionTests, TestClipNotificationAddingOne)
{
    TracksAndClips before = buildTracksAndClips();
    TracksAndClips after = buildTracksAndClips();

    addClipToTrack(after, after.first.back().id, after.second.back().size());

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(1);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    m_au3ChangeDetection->notifyOfUndoRedo(before, after);
}

TEST_F(Au3ChangeDetectionTests, TestClipNotificationAddingTwo)
{
    TracksAndClips before = buildTracksAndClips();
    TracksAndClips after = buildTracksAndClips();

    addClipToTrack(after, after.first.front().id, after.second.front().size());
    addClipToTrack(after, after.first.back().id, after.second.back().size());

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(2);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    m_au3ChangeDetection->notifyOfUndoRedo(before, after);
}

TEST_F(Au3ChangeDetectionTests, TestClipNotificationRemovingOne)
{
    TracksAndClips before = buildTracksAndClips();
    TracksAndClips after = buildTracksAndClips();

    after.second.back().pop_back();

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(1);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    m_au3ChangeDetection->notifyOfUndoRedo(before, after);
}

TEST_F(Au3ChangeDetectionTests, TestClipNotificationRemovingTwo)
{
    TracksAndClips before = buildTracksAndClips();
    TracksAndClips after = buildTracksAndClips();

    after.second.front().pop_back();
    after.second.back().pop_back();

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(2);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    m_au3ChangeDetection->notifyOfUndoRedo(before, after);
}

TEST_F(Au3ChangeDetectionTests, TestClipNotificationChangeStartTime)
{
    TracksAndClips before = buildTracksAndClips();
    TracksAndClips after = buildTracksAndClips();

    after.second.back().back().startTime += 200;

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    m_au3ChangeDetection->notifyOfUndoRedo(before, after);
}

TEST_F(Au3ChangeDetectionTests, TestClipNotificationChangeEndTime)
{
    TracksAndClips before = buildTracksAndClips();
    TracksAndClips after = buildTracksAndClips();

    after.second.back().back().endTime += after.second.back().back().startTime + 200;

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    m_au3ChangeDetection->notifyOfUndoRedo(before, after);
}

TEST_F(Au3ChangeDetectionTests, TestClipNotificationChangeStereo)
{
    TracksAndClips before = buildTracksAndClips();
    TracksAndClips after = buildTracksAndClips();

    after.second.back().back().stereo = !after.second.back().back().stereo;

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    m_au3ChangeDetection->notifyOfUndoRedo(before, after);
}

TEST_F(Au3ChangeDetectionTests, TestClipNotificationChangePitch)
{
    TracksAndClips before = buildTracksAndClips();
    TracksAndClips after = buildTracksAndClips();

    after.second.back().back().pitch = 100;

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    m_au3ChangeDetection->notifyOfUndoRedo(before, after);
}

TEST_F(Au3ChangeDetectionTests, TestClipNotificationChangeSpeed)
{
    TracksAndClips before = buildTracksAndClips();
    TracksAndClips after = buildTracksAndClips();

    after.second.back().back().speed = 10.0;

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    m_au3ChangeDetection->notifyOfUndoRedo(before, after);
}

TEST_F(Au3ChangeDetectionTests, TestClipNotificationChangeGroup)
{
    TracksAndClips before = buildTracksAndClips();
    TracksAndClips after = buildTracksAndClips();

    after.second.back().back().groupId = 2;
    after.second.back().front().groupId = 2;

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(2);

    m_au3ChangeDetection->notifyOfUndoRedo(before, after);
}
}
