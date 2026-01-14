/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "../internal/au3/au3clipsinteraction.h"
#include "../internal/au3/au3trackdata.h"

#include "au3interactiontestbase.h"
#include "mocks/selectioncontrollermock.h"
#include "mocks/trackeditconfigurationmock.h"
#include "mocks/projecthistorymock.h"

#include "global/tests/mocks/interactivemock.h"

using ::testing::Truly;
using ::testing::_;

namespace au::trackedit {
class Au3ClipsInteractionTests : public Au3InteractionTestBase
{
public:
    void SetUp() override
    {
        m_clipsInteraction = std::make_shared<Au3ClipsInteraction>(muse::modularity::globalCtx());

        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_selectionController = std::make_shared<NiceMock<SelectionControllerMock> >();
        m_interactive = std::make_shared<NiceMock<muse::InteractiveMock> >();
        m_playbackState = std::make_shared<NiceMock<context::PlaybackStateMock> >();
        m_configuration = std::make_shared<NiceMock<TrackeditConfigurationMock> >();
        m_projectHistory = std::make_shared<NiceMock<ProjectHistoryMock> >();

        m_clipsInteraction->globalContext.set(m_globalContext);
        m_clipsInteraction->selectionController.set(m_selectionController);
        m_clipsInteraction->interactive.set(m_interactive);
        m_clipsInteraction->configuration.set(m_configuration);
        m_clipsInteraction->projectHistory.set(m_projectHistory);

        m_trackEditProject = std::make_shared<NiceMock<TrackeditProjectMock> >();
        ON_CALL(*m_globalContext, currentTrackeditProject())
        .WillByDefault(Return(m_trackEditProject));

        m_currentProject = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_currentProject));
        ON_CALL(*m_globalContext, playbackState())
        .WillByDefault(Return(m_playbackState));

        ON_CALL(*m_currentProject, trackeditProject())
        .WillByDefault(Return(m_trackEditProject));

        initTestProject();
    }

    std::shared_ptr<Au3ClipsInteraction> m_clipsInteraction;
    std::shared_ptr<SelectionControllerMock> m_selectionController;
    std::shared_ptr<TrackeditConfigurationMock> m_configuration;
    std::shared_ptr<ProjectHistoryMock> m_projectHistory;
    std::shared_ptr<muse::IInteractive> m_interactive;
};

TEST_F(Au3ClipsInteractionTests, ChangeClipColor)
{
    //! [GIVEN] There is a project with a track and a clip
    const TrackId trackMinSilenceId = createTrack(TestTrackID::TRACK_MIN_SILENCE);
    ASSERT_NE(trackMinSilenceId, INVALID_TRACK) << "Failed to create track";

    Au3Project& project = projectRef();
    const Au3WaveTrack* au3WaveTrack = DomAccessor::findWaveTrackByIndex(project, 0);
    const std::shared_ptr<Au3WaveClip> au3Clip = DomAccessor::findWaveClip(project, au3WaveTrack->GetId(), 0);

    //! [THEN] The track edit project should notify about the clip change
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(Truly([=](const Clip& clip) {
        return clip.key == ClipKey { au3WaveTrack->GetId(), au3Clip->GetId() };
    })));

    //! [WHEN] Change the color of the clip
    m_clipsInteraction->changeClipColor(ClipKey { au3WaveTrack->GetId(), au3Clip->GetId() }, "red");

    //! [THEN] The color is updated
    const std::shared_ptr<Au3WaveClip> au3UpdatedClip = DomAccessor::findWaveClip(project, au3WaveTrack->GetId(), 0);
    EXPECT_EQ(au3UpdatedClip->GetColor(), "red");

    // Cleanup
    removeTrack(trackMinSilenceId);
}

TEST_F(Au3ClipsInteractionTests, ClipColorRetainedWhenClipIsCopied)
{
    //! [GIVEN] There is a project with a track and a clip with a custom color
    const TrackId trackId = createTrack(TestTrackID::TRACK_MIN_SILENCE);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3Project& project = projectRef();
    const Au3WaveTrack* track = DomAccessor::findWaveTrack(project, Au3TrackId(trackId));
    const std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(project, track->GetId(), 0);

    m_clipsInteraction->changeClipColor(ClipKey { track->GetId(), clip->GetId() }, "red");

    const Au3Track::Holder trackCopy = track->Copy(clip->GetSequenceStartTime(), clip->GetSequenceEndTime());
    ASSERT_NE(trackCopy, nullptr) << "Failed to copy clip";
    const ITrackDataPtr trackData = std::make_shared<Au3TrackData>(trackCopy);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3ClipsInteractionTests, SplitClipsAtSilencesOnValidInterval)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const TrackId trackMinSilenceId = createTrack(TestTrackID::TRACK_MIN_SILENCE);
    ASSERT_NE(trackMinSilenceId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackMinSilenceId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin the clip
    const WaveTrack::IntervalConstHolder clip = track->GetSortedClipByIndex(0);
    m_clipsInteraction->splitClipsAtSilences({ { track->GetId(), clip->GetId() } });

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "The number of intervals after the split range operation is not 2";

    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(firstClip, TRACK_MIN_SILENCE_CLIP_START, TRACK_MIN_SILENCE_CLIP_END, TRACK_MIN_SILENCE_CLIP_START,
                           TRACK_MIN_SILENCE_CLIP_START + TRACK_MIN_SILENCE_FIRST_SEGMENT_DURATION);

    const WaveTrack::IntervalConstHolder secondClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(secondClip, TRACK_MIN_SILENCE_CLIP_START, TRACK_MIN_SILENCE_CLIP_END,
                           TRACK_MIN_SILENCE_CLIP_START + TRACK_MIN_SILENCE_FIRST_SEGMENT_DURATION + TRACK_MIN_SILENCE_SILENCE_SEGMENT_DURATION,
                           TRACK_MIN_SILENCE_CLIP_END);

    // Cleanup
    removeTrack(trackMinSilenceId);
}

TEST_F(Au3ClipsInteractionTests, SplitClipsAtSilencesOnIntervalWithShortSilence)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const TrackId trackSmallSilenceId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackSmallSilenceId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackSmallSilenceId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin the clip with a small silence
    const WaveTrack::IntervalConstHolder clip = track->GetSortedClipByIndex(0);
    m_clipsInteraction->splitClipsAtSilences({ { track->GetId(), clip->GetId() } });

    //! [THEN] The number of intervals is 1 once the silence is less than 0.01s
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split range operation is not 1";

    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(firstClip, TRACK_SMALL_SILENCE_CLIP_START, TRACK_SMALL_SILENCE_CLIP_END);

    // Cleanup
    removeTrack(trackSmallSilenceId);
}

TEST_F(Au3ClipsInteractionTests, SplitClipsAtSilenceWhenSilenceAtStart)
{
    //! [GIVEN] There is a project with a track and a clip with silence at the start
    const TrackId trackSilenceAtStartId = createTrack(TestTrackID::TRACK_SILENCE_AT_START);
    ASSERT_NE(trackSilenceAtStartId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackSilenceAtStartId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin the clip
    const WaveTrack::IntervalConstHolder clip = track->GetSortedClipByIndex(0);
    m_clipsInteraction->splitClipsAtSilences({ { track->GetId(), clip->GetId() } });

    //! [THEN] The number of intervals is 1 once the silence is at the start
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split range operation is not 1";

    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(firstClip, TRACK_SILENCE_AT_START_CLIP_START, TRACK_SILENCE_AT_START_CLIP_END,
                           TRACK_SILENCE_AT_START_CLIP_START + TRACK_SILENCE_AT_START_SILENCE_DURATION,
                           TRACK_SILENCE_AT_START_CLIP_END);

    // Cleanup
    removeTrack(trackSilenceAtStartId);
}

TEST_F(Au3ClipsInteractionTests, SplitClipsAtSilenceWhenSilenceAtEnd)
{
    //! [GIVEN] There is a project with a track and a clip with silence at the end
    const TrackId trackSilenceAtEndId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackSilenceAtEndId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackSilenceAtEndId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin the clip
    const WaveTrack::IntervalConstHolder clip = track->GetSortedClipByIndex(0);
    m_clipsInteraction->splitClipsAtSilences({ { track->GetId(), clip->GetId() } });

    //! [THEN] The number of intervals is 1 once the silence is at the end
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split range operation is not 1";

    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(firstClip, TRACK_SILENCE_AT_START_CLIP_START, TRACK_SILENCE_AT_START_CLIP_END, TRACK_SILENCE_AT_START_CLIP_START,
                           TRACK_SILENCE_AT_START_CLIP_START + TRACK_SILENCE_AT_START_FIRST_SEGMENT_DURATION);

    // Cleanup
    removeTrack(trackSilenceAtEndId);
}

TEST_F(Au3ClipsInteractionTests, SplitRangeClipsIntoNewTracks)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    //! [THEN] We have 1 tracks
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 1) << "Precondition failed: The number of tracks is not 1";

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Splitting into new tracks
    const WaveTrack::IntervalConstHolder clip1 = track->GetSortedClipByIndex(0);
    const WaveTrack::IntervalConstHolder clip2 = track->GetSortedClipByIndex(1);
    m_clipsInteraction->splitClipsIntoNewTracks({ { track->GetId(), clip1->GetId() } });

    //! [THEN] The number of intervals now is 1
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split range operation is not 1";

    //! [THEN] Now we have added a new track
    ASSERT_EQ(projectTracks.Size(),  2) << "The number of tracks after the split range operation is not 2";

    //! [THEN] The original track has only the second clip
    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(firstClip, TRACK_TWO_CLIPS_CLIP2_START, TRACK_TWO_CLIPS_CLIP2_END);

    //! [THEN] The new track has the first clip
    auto newTrack = projectTracks.rbegin();
    auto newTrackId = (*newTrack)->GetId();
    Au3WaveTrack* newTrackWave = DomAccessor::findWaveTrack(projectRef(), newTrackId);
    ASSERT_EQ(newTrackWave->NIntervals(), 1) << "The number of intervals in the new track is not 1";

    const WaveTrack::IntervalConstHolder newClip = newTrackWave->GetSortedClipByIndex(0);
    ValidateClipProperties(newClip, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP1_END);

    // Cleanup
    removeTrack(newTrackId);
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3ClipsInteractionTests, RemoveSingleClipFromATrack)
{
    //! [GIVEN] There is a project with a track and a clip
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Remove the clip
    const WaveTrack::IntervalConstHolder clip = track->GetSortedClipByIndex(0);
    m_clipsInteraction->removeClip({ track->GetId(), clip->GetId() });

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the remove operation is not 1";

    //! [THEN] The remaining clip keeps the same properties
    const WaveTrack::IntervalConstHolder remainingClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(remainingClip, TRACK_TWO_CLIPS_CLIP2_START, TRACK_TWO_CLIPS_CLIP2_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3ClipsInteractionTests, RemoveTwoClipsFromATrack)
{
    //! [GIVEN] There is a project with a track and two clips
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Remove the clips
    const WaveTrack::IntervalConstHolder clip1 = track->GetSortedClipByIndex(0);
    const WaveTrack::IntervalConstHolder clip2 = track->GetSortedClipByIndex(1);
    m_clipsInteraction->removeClips({ { track->GetId(), clip1->GetId() }, { track->GetId(), clip2->GetId() } }, false);

    //! [THEN] The number of intervals is 0
    ASSERT_EQ(track->NIntervals(), 0) << "The number of intervals after the remove operation is not 0";

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3ClipsInteractionTests, RemoveClipsMovingRemaining)
{
    //! [GIVEN] There is a project with a track and two clips
    const TrackId trackThreeClipsId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackThreeClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackThreeClipsId));

    //! [THEN] The number of intervals is 3
    ASSERT_EQ(track->NIntervals(), 3) << "Precondition failed: The number of intervals is not 3";

    //! [WHEN] Remove the second clip
    const WaveTrack::IntervalConstHolder clip = track->GetSortedClipByIndex(1);
    m_clipsInteraction->removeClips({ { track->GetId(), clip->GetId() } }, true);

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "The number of intervals after the remove operation is not 2";

    //! [THEN] The first clip keeps the same properties
    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(firstClip, TRACK_THREE_CLIPS_CLIP1_START, TRACK_THREE_CLIPS_CLIP1_END);

    //! [THEN] The third clip is moved forward
    const WaveTrack::IntervalConstHolder thirdClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(thirdClip, TRACK_THREE_CLIPS_CLIP2_START, TRACK_THREE_CLIPS_CLIP2_START + TRACK_THREE_CLIPS_CLIP3_DURATION,
                           TRACK_THREE_CLIPS_CLIP2_START,
                           TRACK_THREE_CLIPS_CLIP2_START + TRACK_THREE_CLIPS_CLIP3_DURATION);

    // Cleanup
    removeTrack(trackThreeClipsId);
}

TEST_F(Au3ClipsInteractionTests, CopyClip)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder clip = track->GetSortedClipByIndex(0);
    const ClipKey clipKey { track->GetId(), clip->GetId() };

    //! [WHEN] Copy the tracks
    m_clipsInteraction->copyClip(clipKey);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3ClipsInteractionTests, CutClip)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder clip = track->GetSortedClipByIndex(1);
    const double middleClipStart = clip->GetSequenceStartTime();

    //! [GIVEN] Thee number of interval is 3
    ASSERT_EQ(track->NIntervals(), 3) << "Precondition failed: The number of intervals is not 3";

    const ClipKey clipKey { track->GetId(), clip->GetId() };

    //! [EXPECT] The project is notified about clip removed and track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(1);
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(Truly([&](const Track& modifiedTrack) {
        return modifiedTrack.id == trackId;
    }))).Times(1);

    //! [WHEN] Cut the tracks into the clipboard
    m_clipsInteraction->cutClip(clipKey);

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "The number of intervals after the cut operation is not 2";

    //! [THEN] The last clip was moved
    const WaveTrack::IntervalConstHolder lastClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(lastClip, middleClipStart, middleClipStart + TRACK_THREE_CLIPS_CLIP3_DURATION);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3ClipsInteractionTests, DuplicateClipsOnEmptyList)
{
    //! [GIVEN] A project without tracks
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 0) << "Precondition failed: The number of tracks is not 0";

    //! [EXPECT] Notify about track inserted is not called
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackInserted(_, _)).Times(0);

    //! [WHEN] Duplicate the clips with an empty list
    EXPECT_EQ(m_clipsInteraction->duplicateClips({}), false);

    //! [THEN] The number of tracks is still 0
    ASSERT_EQ(projectTracks.Size(), 0) << "The number of tracks after the duplicate operation is not 0";
}

TEST_F(Au3ClipsInteractionTests, DuplicateSingleClip)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder clip = track->GetSortedClipByIndex(0);

    //! [EXPECT] The number of tracks is 1
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 1) << "Precondition failed: The number of tracks is not 1";

    //! [EXPECT] The trackList is requested
    EXPECT_CALL(*m_trackEditProject, trackIdList()).Times(1).WillOnce(Return(std::vector<trackedit::TrackId> { track->GetId() }));

    //! [EXPECT] Notify about track added
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackAdded(_)).Times(1);

    //! [WHEN] Duplicate the clip
    m_clipsInteraction->duplicateClip({ track->GetId(), clip->GetId() });

    //! [THEN] The number of tracks is 2
    ASSERT_EQ(projectTracks.Size(), 2) << "The number of tracks after the duplicate operation is not 2";

    //Cleanup
    removeTrack(trackId);
    const TrackId newTrackId = (*projectTracks.begin())->GetId();
    removeTrack(newTrackId);
}

TEST_F(Au3ClipsInteractionTests, MoveClipsRight)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    const double secondsToMove = 500 * SAMPLE_INTERVAL;

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    const double firstClipStart = firstClip->GetSequenceStartTime();
    const double firstClipEnd = firstClip->GetSequenceEndTime();

    const WaveTrack::IntervalConstHolder middleClip = track->GetSortedClipByIndex(1);
    const double middleClipStart = middleClip->GetSequenceStartTime();
    const double middleClipEnd = middleClip->GetSequenceEndTime();

    const WaveTrack::IntervalConstHolder lastClip = track->GetSortedClipByIndex(2);
    const double lastClipStart = lastClip->GetSequenceStartTime();
    const double lastClipEnd = lastClip->GetSequenceEndTime();

    ClipKeyList selectedClipsKeys = {
        ClipKey { trackId, firstClip->GetId() },
        ClipKey { trackId, middleClip->GetId() },
        ClipKey { trackId, lastClip->GetId() }
    };

    //! [WHEN] Move the clips right
    auto clipsMovedToOtherTracks = false;
    m_clipsInteraction->moveClips(selectedClipsKeys, secondsToMove, 0, true, clipsMovedToOtherTracks);

    //! [THEN] All clips are moved
    const WaveTrack::IntervalConstHolder modifiedFirstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(modifiedFirstClip, firstClipStart + secondsToMove, firstClipEnd + secondsToMove);
    const WaveTrack::IntervalConstHolder modifiedMiddleClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(modifiedMiddleClip, middleClipStart + secondsToMove, middleClipEnd + secondsToMove);
    const WaveTrack::IntervalConstHolder modifiedLastClip = track->GetSortedClipByIndex(2);
    ValidateClipProperties(modifiedLastClip, lastClipStart + secondsToMove, lastClipEnd + secondsToMove);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3ClipsInteractionTests, MoveClipLeftWhenClipIsAtZero)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    const double firstClipStart = firstClip->GetSequenceStartTime();
    const double firstClipEnd = firstClip->GetSequenceEndTime();

    const WaveTrack::IntervalConstHolder middleClip = track->GetSortedClipByIndex(1);
    const double middleClipStart = middleClip->GetSequenceStartTime();
    const double middleClipEnd = middleClip->GetSequenceEndTime();

    const WaveTrack::IntervalConstHolder lastClip = track->GetSortedClipByIndex(2);
    const double lastClipStart = lastClip->GetSequenceStartTime();
    const double lastClipEnd = lastClip->GetSequenceEndTime();

    ClipKeyList selectedClipsKeys = {
        ClipKey { trackId, firstClip->GetId() },
        ClipKey { trackId, middleClip->GetId() },
        ClipKey { trackId, lastClip->GetId() }
    };

    //! [WHEN] Move the clips left
    auto clipsMovedToOtherTracks = false;
    m_clipsInteraction->moveClips(selectedClipsKeys, -1.0, 0, true, clipsMovedToOtherTracks);

    //! [THEN] No clip is moved
    const WaveTrack::IntervalConstHolder modifiedFirstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(modifiedFirstClip, firstClipStart, firstClipEnd);
    const WaveTrack::IntervalConstHolder modifiedMiddleClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(modifiedMiddleClip, middleClipStart, middleClipEnd);
    const WaveTrack::IntervalConstHolder modifiedLastClip = track->GetSortedClipByIndex(2);
    ValidateClipProperties(modifiedLastClip, lastClipStart, lastClipEnd);

    removeTrack(trackId);
}

TEST_F(Au3ClipsInteractionTests, SplitDeteleByClipId)
{
    //! [GIVEN] There is a project with a track and two clips
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));
    const WaveTrack::IntervalConstHolder firstClip =  track->GetSortedClipByIndex(0);

    //! [WHEN] Split and delete the whole clip
    m_clipsInteraction->clipSplitDelete({ trackTwoClipsId, firstClip->GetId() });

    //! [THEN] Clip was removed there is ony one left
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split delete operation is not 1";

    //! [THEN] The remaining clip stays unchanged
    const WaveTrack::IntervalConstHolder remainingClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(remainingClip, TRACK_TWO_CLIPS_CLIP2_START, TRACK_TWO_CLIPS_CLIP2_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3ClipsInteractionTests, SplitCutByClipId)
{
    //! [GIVEN] There is a project with a track and two clips
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));
    const WaveTrack::IntervalConstHolder firstClip =  track->GetSortedClipByIndex(0);

    //! [WHEN] Split and cut the whole clip
    m_clipsInteraction->clipSplitCut({ trackTwoClipsId, firstClip->GetId() });

    //! [THEN] Clip was removed there is ony one left
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split delete operation is not 1";

    //! [THEN] The remaining clip stays unchanged
    const WaveTrack::IntervalConstHolder remainingClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(remainingClip, TRACK_TWO_CLIPS_CLIP2_START, TRACK_TWO_CLIPS_CLIP2_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3ClipsInteractionTests, TrimSingleClipLeft)
{
    //! [GIVEN] There is a project with a track and a single clip
    const TrackId trackId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder firstClip =  track->GetSortedClipByIndex(0);

    //! [WHEN] Trim the clip from the left
    const secs_t deltaSec = 2 * SAMPLE_INTERVAL;
    const secs_t minClipDuration = 2 * SAMPLE_INTERVAL;
    m_clipsInteraction->trimClipLeft({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clip is trimmed
    const WaveTrack::IntervalConstHolder trimmedClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(trimmedClip, TRACK_SILENCE_AT_END_CLIP_START, TRACK_SILENCE_AT_END_CLIP_END,
                           TRACK_SILENCE_AT_END_CLIP_START + deltaSec, TRACK_SILENCE_AT_END_CLIP_END);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3ClipsInteractionTests, TrimSingleClipRight)
{
    //! [GIVEN] There is a project with a track and a single clip
    const TrackId trackId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder firstClip =  track->GetSortedClipByIndex(0);

    //! [WHEN] Trim the clip from the right
    const secs_t deltaSec = 2 * SAMPLE_INTERVAL;
    const secs_t minClipDuration = 2 * SAMPLE_INTERVAL;
    m_clipsInteraction->trimClipRight({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clip is trimmed
    const WaveTrack::IntervalConstHolder trimmedClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(trimmedClip, TRACK_SILENCE_AT_END_CLIP_START, TRACK_SILENCE_AT_END_CLIP_END, TRACK_SILENCE_AT_END_CLIP_START,
                           TRACK_SILENCE_AT_END_CLIP_END - deltaSec);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3ClipsInteractionTests, TrimTwoClipsLeftShouldConsiderMinClipDuration)
{
    //! [GIVEN] There is a project with a track and two clips
    const TrackId trackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(2);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder firstClip =  track->GetSortedClipByIndex(0);
    const WaveTrack::IntervalConstHolder secondClip =  track->GetSortedClipByIndex(1);

    //! [GIVEN] Both clips are selected
    ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(ClipKeyList {
            ClipKey { trackId, firstClip->GetId() },
            ClipKey { trackId, secondClip->GetId() }
        }));

    //! [WHEN] Trim the clips from the left
    const secs_t deltaSec = TRACK_TWO_CLIPS_CLIP1_DURATION;
    const secs_t minClipDuration = 2 * SAMPLE_INTERVAL;
    m_clipsInteraction->trimClipLeft({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clips are trimmed
    const WaveTrack::IntervalConstHolder trimmedFirstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(trimmedFirstClip, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP1_END,
                           TRACK_TWO_CLIPS_CLIP1_END - minClipDuration, TRACK_TWO_CLIPS_CLIP1_END);

    const WaveTrack::IntervalConstHolder trimmedSecondClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(trimmedSecondClip, TRACK_TWO_CLIPS_CLIP2_START, TRACK_TWO_CLIPS_CLIP2_END,
                           TRACK_TWO_CLIPS_CLIP2_END - minClipDuration, TRACK_TWO_CLIPS_CLIP2_END);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3ClipsInteractionTests, TrimTwoClipsRightShouldConsiderMinClipDuration)
{
    //! [GIVEN] There is a project with a track and two clips
    const TrackId trackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(2);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder firstClip =  track->GetSortedClipByIndex(0);
    const WaveTrack::IntervalConstHolder secondClip =  track->GetSortedClipByIndex(1);

    //! [GIVEN] Both clips are selected
    ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(ClipKeyList {
            ClipKey { trackId, firstClip->GetId() },
            ClipKey { trackId, secondClip->GetId() }
        }));

    //! [WHEN] Trim the clips from the right
    const secs_t deltaSec = TRACK_TWO_CLIPS_CLIP1_DURATION;
    const secs_t minClipDuration = 2 * SAMPLE_INTERVAL;
    m_clipsInteraction->trimClipRight({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clips are trimmed
    const WaveTrack::IntervalConstHolder trimmedFirstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(trimmedFirstClip, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP1_END, TRACK_TWO_CLIPS_CLIP1_START,
                           TRACK_TWO_CLIPS_CLIP1_START + minClipDuration);

    const WaveTrack::IntervalConstHolder trimmedSecondClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(trimmedSecondClip, TRACK_TWO_CLIPS_CLIP2_START, TRACK_TWO_CLIPS_CLIP2_END, TRACK_TWO_CLIPS_CLIP2_START,
                           TRACK_TWO_CLIPS_CLIP2_START + minClipDuration);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3ClipsInteractionTests, StretchSingleClipLeft)
{
    //! [GIVEN] There is a project with a track and a single clip
    const TrackId trackId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder firstClip =  track->GetSortedClipByIndex(0);

    //! [WHEN] Stretch the clip from the left
    //! NOTE: In order to avoid rounding problems while comparing clip properties we stretch the clip by half
    const secs_t deltaSec = TRACK_SILENCE_AT_END_CLIP_DURATION / 2;
    const secs_t minClipDuration = 2 * SAMPLE_INTERVAL;
    m_clipsInteraction->stretchClipLeft({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clip is stretched
    const WaveTrack::IntervalConstHolder stretchedClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(stretchedClip, TRACK_SILENCE_AT_END_CLIP_START + deltaSec, TRACK_SILENCE_AT_END_CLIP_END);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3ClipsInteractionTests, StretchSingleClipRight)
{
    //! [GIVEN] There is a project with a track and a single clip
    const TrackId trackId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder firstClip =  track->GetSortedClipByIndex(0);

    //! [WHEN] Stretch the clip from the right
    //! NOTE: In order to avoid rounding problems while comparing clip properties we stretch the clip by half
    const secs_t deltaSec = TRACK_SILENCE_AT_END_CLIP_DURATION / 2;
    const secs_t minClipDuration = 2 * SAMPLE_INTERVAL;
    m_clipsInteraction->stretchClipRight({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clip is stretched
    const WaveTrack::IntervalConstHolder stretchedClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(stretchedClip, TRACK_SILENCE_AT_END_CLIP_START, TRACK_SILENCE_AT_END_CLIP_END - deltaSec);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3ClipsInteractionTests, StretchTwoClipsLeftShouldConsiderMinClipDuration)
{
    //! [GIVEN] There is a project with a track and two clips
    const TrackId trackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(2);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder firstClip =  track->GetSortedClipByIndex(0);
    const WaveTrack::IntervalConstHolder secondClip =  track->GetSortedClipByIndex(1);

    //! [GIVEN] Both clips are selected
    ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(ClipKeyList {
            ClipKey { trackId, firstClip->GetId() },
            ClipKey { trackId, secondClip->GetId() }
        }));

    //! [WHEN] Stretch the clips from the left
    //! NOTE: In order to avoid rounding problems while comparing clip properties we stretch the clip by half
    const secs_t deltaSec = TRACK_TWO_CLIPS_CLIP1_DURATION;
    const secs_t minClipDuration = TRACK_TWO_CLIPS_CLIP1_DURATION / 2;

    m_clipsInteraction->stretchClipLeft({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clips are stretched considering the minimum clip duration
    const WaveTrack::IntervalConstHolder stretchedFirstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(stretchedFirstClip, TRACK_TWO_CLIPS_CLIP1_START + minClipDuration, TRACK_TWO_CLIPS_CLIP1_END);

    const WaveTrack::IntervalConstHolder stretchedSecondClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(stretchedSecondClip, TRACK_TWO_CLIPS_CLIP2_START + minClipDuration, TRACK_TWO_CLIPS_CLIP2_END);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3ClipsInteractionTests, StretchTwoClipsRightShouldConsiderMinClipDuration)
{
    //! [GIVEN] There is a project with a track and two clips
    const TrackId trackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed twice
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(2);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder firstClip =  track->GetSortedClipByIndex(0);
    const WaveTrack::IntervalConstHolder secondClip =  track->GetSortedClipByIndex(1);

    //! [GIVEN] Both clips are selected
    ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(ClipKeyList {
            ClipKey { trackId, firstClip->GetId() },
            ClipKey { trackId, secondClip->GetId() }
        }));

    //! [WHEN] Stretch the clips from the right
    //! NOTE: In order to avoid rounding problems while comparing clip properties we stretch the clip by half
    const secs_t deltaSec = TRACK_TWO_CLIPS_CLIP1_DURATION;
    const secs_t minClipDuration = TRACK_TWO_CLIPS_CLIP1_DURATION / 2;

    m_clipsInteraction->stretchClipRight({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clips are stretched considering the minimum clip duration
    const WaveTrack::IntervalConstHolder stretchedFirstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(stretchedFirstClip, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP1_END - minClipDuration);

    const WaveTrack::IntervalConstHolder stretchedSecondClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(stretchedSecondClip, TRACK_TWO_CLIPS_CLIP2_START, TRACK_TWO_CLIPS_CLIP2_END - minClipDuration);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3ClipsInteractionTests, IncreaseClipSpeed)
{
    //! [GIVEN] There is a project with a track and a single clip
    const TrackId trackId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder firstClip =  track->GetSortedClipByIndex(0);

    //! [WHEN] Increase the clip speed
    //! TODO: We should rename the parameter from changeClipSpeed once a value of
    //! 2.0 makes the clip expand and not shrink
    const double speedFactor = 2.0;
    m_clipsInteraction->changeClipSpeed({ trackId, firstClip->GetId() }, speedFactor);

    //! [THEN] The clip change the start and end times
    const WaveTrack::IntervalConstHolder modifiedClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(modifiedClip, TRACK_SILENCE_AT_END_CLIP_START,
                           TRACK_SILENCE_AT_END_CLIP_START + TRACK_SILENCE_AT_END_CLIP_DURATION * speedFactor);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3ClipsInteractionTests, DecreaseClipSpeed)
{
    //! [GIVEN] There is a project with a track and a single clip
    const TrackId trackId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder firstClip =  track->GetSortedClipByIndex(0);

    //! [WHEN] Decrease the clip speed
    //! TODO: We should rename the parameter from changeClipSpeed once a value of
    //! 0.5 makes the clip shrink and not expand
    const double speedFactor = 0.5;
    m_clipsInteraction->changeClipSpeed({ trackId, firstClip->GetId() }, speedFactor);

    //! [THEN] The clip change the start and end times
    const WaveTrack::IntervalConstHolder modifiedClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(modifiedClip, TRACK_SILENCE_AT_END_CLIP_START,
                           TRACK_SILENCE_AT_END_CLIP_START + TRACK_SILENCE_AT_END_CLIP_DURATION * speedFactor);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3ClipsInteractionTests, ResetClipSpeed)
{
    //! [GIVEN] There is a project with a track and a single clip
    const TrackId trackId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder firstClip =  track->GetSortedClipByIndex(0);

    //! [GIVEN] The clip speed is not the default
    const double speedFactor = 2.0;
    m_clipsInteraction->changeClipSpeed({ trackId, firstClip->GetId() }, speedFactor);

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    //! [WHEN] Reseting the clip speed
    m_clipsInteraction->resetClipSpeed({ trackId, firstClip->GetId() });

    //! [THEN] The clip start and end time are the same as the original clip
    const WaveTrack::IntervalConstHolder modifiedClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(modifiedClip, TRACK_SILENCE_AT_END_CLIP_START, TRACK_SILENCE_AT_END_CLIP_END);

    // Cleanup
    removeTrack(trackId);
}
}
