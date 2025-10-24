/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "../internal/changedetection.h"

#include "mocks/trackeditprojectmock.h"

using ::testing::NiceMock;
using ::testing::Return;
using ::testing::Truly;
using ::testing::_;

namespace au::trackedit {
class ChangeDetectionTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_trackEditProject = std::make_shared<NiceMock<TrackeditProjectMock> >();

        std::srand(std::time({}));
    }

    void TearDown() override
    {
    }

    static TracksAndItems buildTracksAndClips()
    {
        constexpr int trackCount = 5;

        TracksAndItems structure;

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

    static void addOneTrack(TracksAndItems& structure, int id)
    {
        structure.tracks.emplace_back();
        structure.clips.emplace_back();
        structure.tracks.back().id = id;

        // None of the below should cause a change trigger:
        structure.tracks.back().title = std::to_string(id).c_str();

        switch (id % 2) {
        case 1:
            structure.tracks.back().type = TrackType::Mono;
            break;
        default:
            structure.tracks.back().type = TrackType::Stereo;
            break;

            // The below are unused in the tests. If they're used we can introduce them.
            // structure.tracks.back().type = TrackType::Label;
            // structure.tracks.back().type = TrackType::Undefined;
        }

        structure.tracks.back().color = muse::draw::Color(std::rand() % 256, std::rand() % 256, std::rand() % 256);
    }

    static void addClipToTrack(TracksAndItems& structure, TrackId trackId, int j)
    {
        auto& newClip = structure.clips[trackId].emplace_back();
        newClip.key.itemId = j;
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

    std::shared_ptr<TrackeditProjectMock> m_trackEditProject;
};

TEST_F(ChangeDetectionTests, TestNotificationsWhenTheresNoChanges)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    //! The above are equal. No change notifications are expected.

    EXPECT_EQ(before.tracks.size(), after.tracks.size());

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);

    //! If there are no changes detected,
    //  change detection will reload the project so that it's not out of sync with mode.
    EXPECT_CALL(*m_trackEditProject, reload()).Times(1);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

////////////////////////////////////////////////
/// Tests for track change notification:
////////////////////////////////////////////////

TEST_F(ChangeDetectionTests, TestTrackNotificationsForAddingOneTrack)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    addOneTrack(after, after.tracks.size());

    EXPECT_NE(before.tracks.size(), after.tracks.size());

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(1);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

TEST_F(ChangeDetectionTests, TestTrackNotificationsForAddingTwoTracks)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    addOneTrack(after, static_cast<int>(after.tracks.size()));
    addOneTrack(after, static_cast<int>(after.tracks.size()));

    EXPECT_NE(before.tracks.size(), after.tracks.size());

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(2);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

TEST_F(ChangeDetectionTests, TestTrackNotificationsForRemovingOneTrack)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    after.tracks.pop_back();
    after.clips.pop_back();

    EXPECT_NE(before.tracks.size(), after.tracks.size());

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(1);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

TEST_F(ChangeDetectionTests, TestTrackNotificationsForRemovingTwoTracks)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    after.tracks.pop_back();
    after.clips.pop_back();

    after.tracks.pop_back();
    after.clips.pop_back();

    EXPECT_NE(before.tracks.size(), after.tracks.size());

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(2);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

TEST_F(ChangeDetectionTests, TestTrackNotificationsForReordering)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    EXPECT_EQ(before.tracks.size(), after.tracks.size());

    // Swapping the ID's
    before.tracks.back().id = 0;

    for (Clip& clip : before.clips.back()) {
        clip.key.trackId = 0;
    }

    before.tracks.front().id = 4;

    for (Clip& clip : before.clips.front()) {
        clip.key.trackId = 4;
    }

    // Reordering is detected indirectly, as removal and addition.
    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(1);

    // With the added side effect,
    // that since the ID's pre-existed, clip addition/removal is triggered.
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

TEST_F(ChangeDetectionTests, TestTrackNotificationForTitleChange)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    EXPECT_EQ(before.tracks.size(), after.tracks.size());

    before.tracks.back().title = "new title";

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(1);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

////////////////////////////////////////////////
/// Tests for clip change notification:
////////////////////////////////////////////////

TEST_F(ChangeDetectionTests, TestClipNotificationAddingOne)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    addClipToTrack(after, after.tracks.back().id, static_cast<int>(after.clips.back().size()));

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(1);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

TEST_F(ChangeDetectionTests, TestClipNotificationAddingTwo)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    addClipToTrack(after, after.tracks.front().id, static_cast<int>(after.clips.front().size()));
    addClipToTrack(after, after.tracks.back().id, static_cast<int>(after.clips.back().size()));

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(2);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

TEST_F(ChangeDetectionTests, TestClipNotificationRemovingOne)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    after.clips.back().pop_back();

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(1);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

TEST_F(ChangeDetectionTests, TestClipNotificationRemovingTwo)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    after.clips.front().pop_back();
    after.clips.back().pop_back();

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(2);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

TEST_F(ChangeDetectionTests, TestClipNotificationChangeStartTime)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    after.clips.back().back().startTime += 200;

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

TEST_F(ChangeDetectionTests, TestClipNotificationChangeEndTime)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    after.clips.back().back().endTime += after.clips.back().back().startTime + 200;

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

TEST_F(ChangeDetectionTests, TestClipNotificationChangeStereo)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    if (after.tracks.back().type == TrackType::Stereo) {
        after.tracks.back().type = TrackType::Mono;
    } else {
        after.tracks.back().type = TrackType::Stereo;
    }

    for (auto& clip : after.clips.back()) {
        clip.stereo = !clip.stereo;
    }

    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(1);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(5);

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

TEST_F(ChangeDetectionTests, TestClipNotificationChangePitch)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    after.clips.back().back().pitch = 100;

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

TEST_F(ChangeDetectionTests, TestClipNotificationChangeSpeed)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    after.clips.back().back().speed = 10.0;

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

TEST_F(ChangeDetectionTests, TestClipNotificationChangeGroup)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    after.clips.back().back().groupId = 2;
    after.clips.back().front().groupId = 2;

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(2);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

TEST_F(ChangeDetectionTests, TestClipNotificationChangeVersion)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    after.clips.back().back().clipVersion++;

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

////////////////////////////////////////////////
/// Compound changes:
////////////////////////////////////////////////

TEST_F(ChangeDetectionTests, TestNotificationsForAddingOneTrackAndOneClip)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    addOneTrack(after, after.tracks.size());

    addClipToTrack(after, after.tracks.front().id, static_cast<int>(after.clips.front().size()));

    EXPECT_NE(before.tracks.size(), after.tracks.size());

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(1);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(1);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

TEST_F(ChangeDetectionTests, TestClipNotificationAddingTwoAndRemovingTwo)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    after.clips.front().pop_back();
    after.clips.back().pop_back();

    int newClipIdNumber = static_cast<int>(after.clips.back().size()) + 1;

    addClipToTrack(after, after.tracks.front().id, newClipIdNumber);
    addClipToTrack(after, after.tracks.back().id, newClipIdNumber + 1);

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(2);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(2);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}

TEST_F(ChangeDetectionTests, TestClipNotificationChangeTitle)
{
    TracksAndItems before = buildTracksAndClips();
    TracksAndItems after = buildTracksAndClips();

    after.clips.back().back().title = "new clip title";

    EXPECT_CALL(*m_trackEditProject, trackInserted()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackRemoved()).Times(0);
    EXPECT_CALL(*m_trackEditProject, trackChanged()).Times(0);
    EXPECT_CALL(*m_trackEditProject, reload()).Times(0);

    EXPECT_CALL(*m_trackEditProject, notifyAboutClipAdded(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    changeDetection::notifyOfUndoRedo(before, after, m_trackEditProject);
}
}
