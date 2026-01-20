/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "../internal/au3/au3tracksinteraction.h"
#include "../internal/au3/au3trackdata.h"

#include "au3interactiontestbase.h"
#include "mocks/selectioncontrollermock.h"
#include "mocks/trackeditconfigurationmock.h"
#include "mocks/projecthistorymock.h"
#include "mocks/clipboardmock.h"
#include "../trackediterrors.h"

#include "global/tests/mocks/interactivemock.h"

#include "au3wrap/internal/trackcolor.h"

using ::testing::Truly;
using ::testing::_;

namespace au::trackedit {
class Au3TracksInteractionTests : public Au3InteractionTestBase
{
public:
    void SetUp() override
    {
        m_tracksInteraction = std::make_shared<Au3TracksInteraction>(muse::modularity::globalCtx());

        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_selectionController = std::make_shared<NiceMock<SelectionControllerMock> >();
        m_interactive = std::make_shared<NiceMock<muse::InteractiveMock> >();
        m_playbackState = std::make_shared<NiceMock<context::PlaybackStateMock> >();
        m_configuration = std::make_shared<NiceMock<TrackeditConfigurationMock> >();
        m_projectHistory = std::make_shared<NiceMock<ProjectHistoryMock> >();
        m_clipboard = std::make_shared<NiceMock<ClipboardMock> >();

        m_tracksInteraction->globalContext.set(m_globalContext);
        m_tracksInteraction->selectionController.set(m_selectionController);
        m_tracksInteraction->interactive.set(m_interactive);
        m_tracksInteraction->configuration.set(m_configuration);
        m_tracksInteraction->projectHistory.set(m_projectHistory);

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

    int TrackPosition(const TrackId trackId)
    {
        return m_tracksInteraction->trackPosition(trackId);
    }

    std::shared_ptr<Au3TracksInteraction> m_tracksInteraction;
    std::shared_ptr<SelectionControllerMock> m_selectionController;
    std::shared_ptr<TrackeditConfigurationMock> m_configuration;
    std::shared_ptr<ProjectHistoryMock> m_projectHistory;
    std::shared_ptr<ClipboardMock> m_clipboard;
    std::shared_ptr<muse::IInteractive> m_interactive;
};

TEST_F(Au3TracksInteractionTests, ChangeTrackColor)
{
    //! [GIVEN] There is a project with a track
    const TrackId trackMinSilenceId = createTrack(TestTrackID::TRACK_MIN_SILENCE);
    ASSERT_NE(trackMinSilenceId, INVALID_TRACK) << "Failed to create track";

    Au3Project& project = projectRef();
    const std::shared_ptr<Au3WaveClip> au3Clip = DomAccessor::findWaveClip(project, trackMinSilenceId, 0);

    //! [WHEN] Change the color of the track
    m_tracksInteraction->changeTracksColor({ trackMinSilenceId }, "#48BECF");

    //! [THEN] Clip color is cleared to follow the track color
    const std::shared_ptr<Au3WaveClip> au3UpdatedClip = DomAccessor::findWaveClip(project, trackMinSilenceId, 0);
    EXPECT_EQ(au3UpdatedClip->GetColor(), "");

    //! [THEN] Track color is changed
    const auto& waveTrack = DomAccessor::findWaveTrack(project, Au3TrackId { trackMinSilenceId });
    const auto& trackColor = TrackColor::Get(waveTrack).GetColor();
    EXPECT_EQ(trackColor.toString(), "#48BECF");

    // Cleanup
    removeTrack(trackMinSilenceId);
}

TEST_F(Au3TracksInteractionTests, SplitRangeSelectionAtSilencesOnValidInterval)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const TrackId trackMinSilenceId = createTrack(TestTrackID::TRACK_MIN_SILENCE);
    ASSERT_NE(trackMinSilenceId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackMinSilenceId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin within the clip bondaries
    m_tracksInteraction->splitRangeSelectionAtSilences({ track->GetId() }, TRACK_MIN_SILENCE_CLIP_START, TRACK_MIN_SILENCE_CLIP_END);

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "The number of intervals after the split range operation is not 2";

    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    ASSERT_NE(firstClip, nullptr) << "The first clip is not found";
    ValidateClipProperties(firstClip, TRACK_MIN_SILENCE_CLIP_START, TRACK_MIN_SILENCE_CLIP_END, TRACK_MIN_SILENCE_CLIP_START,
                           TRACK_MIN_SILENCE_CLIP_START + TRACK_MIN_SILENCE_FIRST_SEGMENT_DURATION);

    const WaveTrack::IntervalConstHolder secondClip = track->GetSortedClipByIndex(1);
    ASSERT_NE(secondClip, nullptr) << "The second clip is not found";
    ValidateClipProperties(secondClip, TRACK_MIN_SILENCE_CLIP_START, TRACK_MIN_SILENCE_CLIP_END,
                           TRACK_MIN_SILENCE_CLIP_START + TRACK_MIN_SILENCE_FIRST_SEGMENT_DURATION + TRACK_MIN_SILENCE_SILENCE_SEGMENT_DURATION,
                           TRACK_MIN_SILENCE_CLIP_END);

    // Cleanup
    removeTrack(trackMinSilenceId);
}

TEST_F(Au3TracksInteractionTests, SplitRangeSelectionAtSilencesOnInvalidInterval)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const TrackId trackMinSilenceId = createTrack(TestTrackID::TRACK_MIN_SILENCE);
    ASSERT_NE(trackMinSilenceId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackMinSilenceId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin outside the clip bondaries
    m_tracksInteraction->splitRangeSelectionAtSilences(
        { track->GetId() }, TRACK_MIN_SILENCE_CLIP_END, TRACK_MIN_SILENCE_CLIP_END + 1.0);

    //! [THEN] The number of intervals is still one
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split range operation is not 1";

    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(firstClip, TRACK_MIN_SILENCE_CLIP_START, TRACK_MIN_SILENCE_CLIP_END);

    // Cleanup
    removeTrack(trackMinSilenceId);
}

TEST_F(Au3TracksInteractionTests, SplitRangeSelectionAtSilencesOnIntervalWithShortSilence)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const TrackId trackSmallSilenceId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackSmallSilenceId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackSmallSilenceId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin the clip with a small silence
    m_tracksInteraction->splitRangeSelectionAtSilences({ track->GetId() }, TRACK_SMALL_SILENCE_CLIP_START, TRACK_SMALL_SILENCE_CLIP_END);

    //! [THEN] The number of intervals is 1 once the silence is less than 0.01s
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split range operation is not 1";

    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(firstClip, TRACK_SMALL_SILENCE_CLIP_START, TRACK_SMALL_SILENCE_CLIP_END);

    // Cleanup
    removeTrack(trackSmallSilenceId);
}
TEST_F(Au3TracksInteractionTests, MergeSelectedOnTrackOnValidInterval)
{
    //! [GIVEN] There is a project with a track and two clips
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Merge the clips
    const WaveTrack::IntervalConstHolder clip1 = track->GetSortedClipByIndex(0);
    const WaveTrack::IntervalConstHolder clip2 = track->GetSortedClipByIndex(1);
    m_tracksInteraction->mergeSelectedOnTracks({ track->GetId() }, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP2_END);

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the merge operation is not 1";

    const WaveTrack::IntervalConstHolder mergedClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(mergedClip, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP2_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3TracksInteractionTests, SplitRangeSelectionIntoNewTracks)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    //! [THEN] We have one track
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 1) << "Precondition failed: The number of tracks is not 1";

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Splitting into new tracks
    m_tracksInteraction->splitRangeSelectionIntoNewTracks({ track->GetId() }, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP2_END);

    //! [THEN] The number of intervals now is 0
    ASSERT_EQ(track->NIntervals(), 0) << "The number of intervals after the split range operation is not 0";

    //! [THEN] Now we have added a new track
    ASSERT_EQ(projectTracks.Size(), 2) << "The number of tracks after the split range operation is not 2";

    //! [THEN] The new track has the same clip information as the original one
    auto newTrack = projectTracks.rbegin();
    auto newTrackId = (*newTrack)->GetId();
    Au3WaveTrack* newTrackWave = DomAccessor::findWaveTrack(projectRef(), newTrackId);
    ASSERT_EQ(newTrackWave->NIntervals(), 2) << "The number of intervals in the new track is not 2";

    const WaveTrack::IntervalConstHolder firstClip = newTrackWave->GetSortedClipByIndex(0);
    ValidateClipProperties(firstClip, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP1_END);

    const WaveTrack::IntervalConstHolder secondClip = newTrackWave->GetSortedClipByIndex(1);
    ValidateClipProperties(secondClip, TRACK_TWO_CLIPS_CLIP2_START, TRACK_TWO_CLIPS_CLIP2_END);

    // Cleanup
    removeTrack(newTrackId);
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3TracksInteractionTests, SplitRangeSelectionIntoNewTracksOutOfClipBounds)
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

    //! [WHEN] Splitting into new tracks outside the clip bounds
    m_tracksInteraction->splitRangeSelectionIntoNewTracks({ track->GetId() }, TRACK_TWO_CLIPS_CLIP2_END, TRACK_TWO_CLIPS_CLIP2_END + 1.0);

    //! [THEN] The number of intervals is still 2
    ASSERT_EQ(track->NIntervals(), 2) << "The number of intervals after the split range operation is not 2";

    //! [THEN] The number of tracks is still one
    ASSERT_EQ(projectTracks.Size(), 1) << "The number of tracks after the split range operation is not 1";

    //! [THEN] The clip information is still the same
    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(firstClip, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP1_END);

    const WaveTrack::IntervalConstHolder secondClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(secondClip, TRACK_TWO_CLIPS_CLIP2_START, TRACK_TWO_CLIPS_CLIP2_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3TracksInteractionTests, CopyContinuousTrackData)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));

    //! [WHEN] Copy the tracks inside the clip bounds
    m_tracksInteraction->copyContinuousTrackData(track->GetId(), track->GetClip(0)->GetSequenceStartTime(),
                                                 track->GetClip(0)->GetSequenceEndTime());

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3TracksInteractionTests, CopyContinuousTrackDataOutsideClipBounds)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));

    //! [WHEN] Copy the tracks outside the clip bounds
    m_tracksInteraction->copyContinuousTrackData(track->GetId(), track->GetClip(0)->GetSequenceEndTime() + 1.0,
                                                 track->GetClip(0)->GetSequenceEndTime() + 2.0);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3TracksInteractionTests, CopyContinuousTrackDataThrowsWhenStartIsGreaterThanEnd)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));

    //! [WHEN] Copy the tracks thrown InconsistencyExpection
    ASSERT_THROW(m_tracksInteraction->copyContinuousTrackData(track->GetId(), track->GetClip(0)->GetSequenceEndTime(),
                                                              track->GetClip(0)->GetSequenceStartTime()), InconsistencyException);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3TracksInteractionTests, CopyNonContinuousTrackData)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));

    //! [WHEN] Copy the tracks
    std::vector<ClipKey> clips;
    for (size_t i = 0; i < track->NIntervals(); i++) {
        clips.push_back({ track->GetId(), track->GetClip(i)->GetId() });
    }
    m_tracksInteraction->copyNonContinuousTrackData(track->GetId(), clips, 0);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3TracksInteractionTests, CutTrackDataWithoutMovingClips)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder clip = track->GetSortedClipByIndex(1);
    const double middleClipStart = clip->GetSequenceStartTime();
    const double midleClipEnd = clip->GetSequenceEndTime();

    const WaveTrack::IntervalConstHolder lastClip = track->GetSortedClipByIndex(2);
    const double lastClipStart = lastClip->GetSequenceStartTime();
    const double lastClipEnd = lastClip->GetSequenceEndTime();

    //! [GIVEN] Thee number of interval is 3
    ASSERT_EQ(track->NIntervals(), 3) << "Precondition failed: The number of intervals is not 3";

    //! [EXPECT] The project is notified about clip removed and track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(Truly([&](const Track& modifiedTrack) {
        return modifiedTrack.id == trackId;
    }))).Times(1);

    //! [WHEN] Cut the tracks into the clipboard
    m_tracksInteraction->cutTrackData(trackId, middleClipStart, midleClipEnd, false);

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "The number of intervals after the cut operation is not 2";

    //! [THEN] The last clip was not moved
    const WaveTrack::IntervalConstHolder modifiedLastClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(modifiedLastClip, lastClipStart, lastClipEnd);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3TracksInteractionTests, CutTrackDataMovingClips)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder clip = track->GetSortedClipByIndex(1);
    const double middleClipStart = clip->GetSequenceStartTime();
    const double midleClipEnd = clip->GetSequenceEndTime();

    const WaveTrack::IntervalConstHolder lastClip = track->GetSortedClipByIndex(2);
    const double lastClipDuration = lastClip->GetSequenceEndTime() - lastClip->GetSequenceStartTime();

    //! [GIVEN] Thee number of interval is 3
    ASSERT_EQ(track->NIntervals(), 3) << "Precondition failed: The number of intervals is not 3";

    //! [EXPECT] The project is notified about clip removed and track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(Truly([&](const Track& modifiedTrack) {
        return modifiedTrack.id == trackId;
    }))).Times(1);

    //! [WHEN] Cut the tracks into the clipboard
    m_tracksInteraction->cutTrackData(trackId, middleClipStart, midleClipEnd, true);

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "The number of intervals after the cut operation is not 2";

    //! [THEN] The last clip was moved
    const WaveTrack::IntervalConstHolder modifiedLastClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(modifiedLastClip, middleClipStart, middleClipStart + lastClipDuration);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3TracksInteractionTests, SplitDeleteOnRangeSelection)
{
    //! [GIVEN] There is a project with a track and a two clips
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";

    const double begin = TRACK_TWO_CLIPS_CLIP1_START + 2 * SAMPLE_INTERVAL;
    const double end = TRACK_TWO_CLIPS_CLIP1_START + 4 * SAMPLE_INTERVAL;

    //! [WHEN] Split and delete part of the first clip
    m_tracksInteraction->splitDeleteSelectedOnTracks({ trackTwoClipsId }, begin, end);

    //! [THEN] The number of intervals should be 3
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));
    ASSERT_EQ(track->NIntervals(), 3) << "The number of intervals after the split delete operation is not 3";

    const WaveTrack::IntervalConstHolder firstSplittedClip = track->GetSortedClipByIndex(0);
    const WaveTrack::IntervalConstHolder secondSplittedClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(firstSplittedClip, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP1_END, TRACK_TWO_CLIPS_CLIP1_START, begin);
    ValidateClipProperties(secondSplittedClip, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP1_END, end, TRACK_TWO_CLIPS_CLIP1_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3TracksInteractionTests, SpliCutOnRangeSelection)
{
    //! [GIVEN] There is a project with a track and two clips
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(1);

    const double begin = TRACK_TWO_CLIPS_CLIP1_START + 2 * SAMPLE_INTERVAL;
    const double end = TRACK_TWO_CLIPS_CLIP1_START + 4 * SAMPLE_INTERVAL;

    //! [WHEN] Split and cut part of the first clip
    m_tracksInteraction->splitCutSelectedOnTracks({ trackTwoClipsId }, begin, end);

    //! [THEN] The number of intervals should be 3
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));
    ASSERT_EQ(track->NIntervals(), 3) << "The number of intervals after the split delete operation is not 3";

    //! [THEN] The clip are splitted and the other remains untouched
    const WaveTrack::IntervalConstHolder untouchedClip = track->GetSortedClipByIndex(2);
    ASSERT_NE(untouchedClip, nullptr) << "The untouched clip is not found";
    ValidateClipProperties(untouchedClip, TRACK_TWO_CLIPS_CLIP2_START, TRACK_TWO_CLIPS_CLIP2_END);
    const WaveTrack::IntervalConstHolder firstSplittedClip = track->GetSortedClipByIndex(0);
    ASSERT_NE(firstSplittedClip, nullptr) << "The first splitted clip is not found";
    const WaveTrack::IntervalConstHolder secondSplittedClip = track->GetSortedClipByIndex(1);
    ASSERT_NE(secondSplittedClip, nullptr) << "The second splitted clip is not found";
    ASSERT_NE(firstSplittedClip, secondSplittedClip) << "The first and second splitted clips are the same";
    ValidateClipProperties(firstSplittedClip, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP1_END, TRACK_TWO_CLIPS_CLIP1_START, begin);
    ValidateClipProperties(secondSplittedClip, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP1_END, end, TRACK_TWO_CLIPS_CLIP1_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3TracksInteractionTests, SplitTracksAtEmptyList)
{
    //! [EXPECT] The project is not notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackAdded(_)).Times(0);

    //! [WHEN] Split an empty list
    m_tracksInteraction->splitTracksAt({}, { 0.0 });
}

TEST_F(Au3TracksInteractionTests, SplitTracksOnClipData)
{
    //! [GIVEN] There is a project with a track and two clips
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    //! [WHEN] Split the track in the middle of the first clip
    const secs_t pivot = TRACK_TWO_CLIPS_CLIP1_START + 2 * SAMPLE_INTERVAL;
    m_tracksInteraction->splitTracksAt({ trackTwoClipsId }, { pivot });

    //! [THEN] The number of intervals should be 3
    ASSERT_EQ(track->NIntervals(), 3) << "The number of intervals after the split operation is not 3";

    //! [THEN] The clip are splitted and the other remains untouched
    const WaveTrack::IntervalConstHolder untouchedClip = track->GetSortedClipByIndex(2);
    const WaveTrack::IntervalConstHolder firstSplittedClip = track->GetSortedClipByIndex(0);
    const WaveTrack::IntervalConstHolder secondSplittedClip = track->GetSortedClipByIndex(1);

    ASSERT_NE(untouchedClip, nullptr) << "The untouched clip is not found";
    ASSERT_NE(firstSplittedClip, nullptr) << "The first splitted clip is not found";
    ASSERT_NE(secondSplittedClip, nullptr) << "The second splitted clip is not found";
    ASSERT_NE(firstSplittedClip, secondSplittedClip) << "The first and second splitted clips are the same";

    ValidateClipProperties(untouchedClip, TRACK_TWO_CLIPS_CLIP2_START, TRACK_TWO_CLIPS_CLIP2_END);
    ValidateClipProperties(firstSplittedClip, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP1_END, TRACK_TWO_CLIPS_CLIP1_START, pivot);
    ValidateClipProperties(secondSplittedClip, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP1_END, pivot, TRACK_TWO_CLIPS_CLIP1_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3TracksInteractionTests, RangeSplitTracks)
{
    //! [GIVEN] There is a project with a track and two clips
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    ASSERT_EQ(track->NIntervals(), 2) << "The number of intervals before the split operation is not 2";

    //! [WHEN] Split the track on range selection within 1st clip boundaries
    std::vector<secs_t> pivots;
    pivots.push_back(TRACK_TWO_CLIPS_CLIP1_START + 2 * SAMPLE_INTERVAL);
    pivots.push_back(TRACK_TWO_CLIPS_CLIP1_START + 5 * SAMPLE_INTERVAL);
    m_tracksInteraction->splitTracksAt({ trackTwoClipsId }, { pivots });

    //! [THEN] The clip is splitted in two timepoints
    ASSERT_EQ(track->NIntervals(), 4) << "The number of intervals after the split operation is not 4";

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3TracksInteractionTests, RemoveTracksDataSingleClip)
{
    //! [GIVEN] There is a project with a track and a clip
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Remove the track data
    m_tracksInteraction->removeTracksData({ track->GetId() }, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP1_END, false);

    //! [THEN] The number of intervals is 0
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the remove operation is not 1";

    //! [THEN] The remaining clip keeps the same properties
    const WaveTrack::IntervalConstHolder remainingClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(remainingClip, TRACK_TWO_CLIPS_CLIP2_START, TRACK_TWO_CLIPS_CLIP2_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3TracksInteractionTests, RemoveTracksDataRipplePerClip)
{
    //! [GIVEN] There is a project with a track and a clip
    const TrackId trackThreeClipsId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackThreeClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackThreeClipsId));

    //! [THEN] The number of intervals is 3
    ASSERT_EQ(track->NIntervals(), 3) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Remove part of the second clip
    m_tracksInteraction->removeTracksData({ track->GetId() }, TRACK_THREE_CLIPS_CLIP2_START,
                                          (TRACK_THREE_CLIPS_CLIP2_START + TRACK_THREE_CLIPS_CLIP2_END) / 2, false);

    //! [THEN] The number of intervals is still 3
    ASSERT_EQ(track->NIntervals(), 3) << "The number of intervals after the remove operation is not 2";

    //! [THEN] The first and third clips keep the same properties
    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(firstClip, TRACK_THREE_CLIPS_CLIP1_START, TRACK_THREE_CLIPS_CLIP1_END);
    const WaveTrack::IntervalConstHolder thirdClip = track->GetSortedClipByIndex(2);
    ValidateClipProperties(thirdClip, TRACK_THREE_CLIPS_CLIP3_START, TRACK_THREE_CLIPS_CLIP3_END);

    //! [THEN] The second clip is splitted and moved forward
    const WaveTrack::IntervalConstHolder secondClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(secondClip, TRACK_THREE_CLIPS_CLIP2_START, (TRACK_THREE_CLIPS_CLIP2_START + TRACK_THREE_CLIPS_CLIP2_END) / 2,
                           TRACK_THREE_CLIPS_CLIP2_START,
                           (TRACK_THREE_CLIPS_CLIP2_START + TRACK_THREE_CLIPS_CLIP2_END) / 2);

    // Cleanup
    removeTrack(trackThreeClipsId);
}

TEST_F(Au3TracksInteractionTests, RemoveTracksDataRipplePerTrack)
{
    //! [GIVEN] There is a project with a track and a clip
    const TrackId trackThreeClipsId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackThreeClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackThreeClipsId));

    //! [THEN] The number of intervals is 3
    ASSERT_EQ(track->NIntervals(), 3) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Remove part of the second clip
    m_tracksInteraction->removeTracksData({ track->GetId() }, TRACK_THREE_CLIPS_CLIP2_START,
                                          (TRACK_THREE_CLIPS_CLIP2_START + TRACK_THREE_CLIPS_CLIP2_END) / 2, true);

    //! [THEN] The number of intervals is still 3
    ASSERT_EQ(track->NIntervals(), 3) << "The number of intervals after the remove operation is not 3";

    //! [THEN] The first clip keeps the same properties
    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(firstClip, TRACK_THREE_CLIPS_CLIP1_START, TRACK_THREE_CLIPS_CLIP1_END);

    //! [THEN] The second clip is splitted and moved forward
    const WaveTrack::IntervalConstHolder secondClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(secondClip, TRACK_THREE_CLIPS_CLIP2_START, (TRACK_THREE_CLIPS_CLIP2_START + TRACK_THREE_CLIPS_CLIP2_END) / 2,
                           TRACK_THREE_CLIPS_CLIP2_START,
                           (TRACK_THREE_CLIPS_CLIP2_START + TRACK_THREE_CLIPS_CLIP2_END) / 2);

    //! [THEN] The third clip is moved forward as well
    const WaveTrack::IntervalConstHolder thirdClip = track->GetSortedClipByIndex(2);
    ValidateClipProperties(thirdClip, (TRACK_THREE_CLIPS_CLIP2_START + TRACK_THREE_CLIPS_CLIP2_END) / 2,
                           (TRACK_THREE_CLIPS_CLIP2_START + TRACK_THREE_CLIPS_CLIP2_END) / 2 + TRACK_THREE_CLIPS_CLIP3_DURATION);

    // Cleanup
    removeTrack(trackThreeClipsId);
}

TEST_F(Au3TracksInteractionTests, DeleteTracks)
{
    const TrackId trackThreeClipsId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);

    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 2) << "Precondition failed: The number of tracks is not 2";

    //! [WHEN] Delete one track
    m_tracksInteraction->deleteTracks({ trackThreeClipsId });

    //! [THEN] The number of tracks is decremented
    ASSERT_EQ(projectTracks.Size(), 1) << "The number of tracks after the delete operation is not 4";

    //Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3TracksInteractionTests, DuplicateTracksOnEmptyList)
{
    //! [GIVEN] A project without tracks
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 0) << "Precondition failed: The number of tracks is not 0";

    //! [EXPECT] Notify about track inserted is not called
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackInserted(_, _)).Times(0);

    //! [WHEN] Duplicate the tracks with an empty list
    EXPECT_EQ(m_tracksInteraction->duplicateTracks({}), true);

    //! [THEN] The number of tracks is still 0
    ASSERT_EQ(projectTracks.Size(), 0) << "The number of tracks after the duplicate operation is not 0";
}

TEST_F(Au3TracksInteractionTests, DuplicateTracks)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);

    //! [EXPECT] The number of tracks is 1
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 1) << "Precondition failed: The number of tracks is not 1";

    //! [EXPECT] Notify about track added
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackInserted(_, _)).Times(1);

    //! [WHEN] Duplicate the track
    const TrackIdList trackList { trackId };
    m_tracksInteraction->duplicateTracks(trackList);

    //! [THEN] The number of tracks is 2
    ASSERT_EQ(projectTracks.Size(), 2) << "The number of tracks after the duplicate operation is not 2";

    //Cleanup
    removeTrack(trackId);
    const TrackId newTrackId = (*projectTracks.begin())->GetId();
    removeTrack(newTrackId);
}

TEST_F(Au3TracksInteractionTests, DuplicateRangeSelectionOnNewTrack)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));

    //! [EXPECT] The number of tracks is 1
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 1) << "Precondition failed: The number of tracks is not 1";

    //! [EXPECT] Notify about track added
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackAdded(_)).Times(1);

    //! [WHEN] Duplicate the range selection
    m_tracksInteraction->duplicateSelectedOnTracks({ track->GetId() }, TRACK_THREE_CLIPS_CLIP2_START, TRACK_THREE_CLIPS_CLIP2_END);

    //! [THEN] The number of tracks is 2
    ASSERT_EQ(projectTracks.Size(), 2) << "The number of tracks after the duplicate operation is not 2";

    //! [THEN] The new track has once clip
    const TrackId newTrackId = (*projectTracks.rbegin())->GetId();
    Au3WaveTrack* newTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(newTrackId));
    ASSERT_EQ(newTrack->NIntervals(), 1) << "The number of intervals in the new track is not 1";

    const WaveTrack::IntervalConstHolder newClip = newTrack->GetSortedClipByIndex(0);
    ValidateClipProperties(newClip, TRACK_THREE_CLIPS_CLIP2_START, TRACK_THREE_CLIPS_CLIP2_END);

    //Cleanup
    removeTrack(trackId);
    removeTrack(newTrackId);
}

TEST_F(Au3TracksInteractionTests, MoveOnEmptyList)
{
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackMoved(_, _)).Times(0);
    m_tracksInteraction->moveTracks({}, TrackMoveDirection::Up);
}

TEST_F(Au3TracksInteractionTests, MoveTracksUpSingle)
{
    // [GIVEN] There is a project with two tracks
    const TrackId trackId1 = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId1, INVALID_TRACK) << "Failed to create track";

    const TrackId trackId2 = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId2, INVALID_TRACK) << "Failed to create track 2";

    // [THEN] Track positions are as expected (track1 = 0, track2 = 1)
    ASSERT_EQ(TrackPosition(trackId1), 0) << "Track 1 is not at position 0";
    ASSERT_EQ(TrackPosition(trackId2), 1) << "Track 2 is not at position 1";

    //! [EXPECT] Notify about track moved
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackMoved(_, 0)).Times(1);

    // [WHEN] Moving track2 up
    m_tracksInteraction->moveTracks({ trackId2 }, TrackMoveDirection::Up);

    // [THEN] Track positions are swapped
    EXPECT_EQ(TrackPosition(trackId1), 1) << "Track 1 is not at position 1";
    EXPECT_EQ(TrackPosition(trackId2), 0) << "Track 2 is not at position 0";

    // Cleanup
    removeTrack(trackId1);
    removeTrack(trackId2);
}

TEST_F(Au3TracksInteractionTests, MoveTrackDownSingle)
{
    // [GIVEN] There is a project with two tracks
    const TrackId trackId1 = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId1, INVALID_TRACK) << "Failed to create track";

    const TrackId trackId2 = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId2, INVALID_TRACK) << "Failed to create track 2";

    // [THEN] Track positions are as expected (track1 = 0, track2 = 1)
    ASSERT_EQ(TrackPosition(trackId1), 0) << "Track 1 is not at position 0";
    ASSERT_EQ(TrackPosition(trackId2), 1) << "Track 2 is not at position 1";

    //! [EXPECT] Notify about track moved
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackMoved(_, 1)).Times(1);

    // [WHEN] Moving track1 down
    m_tracksInteraction->moveTracks({ trackId1 }, TrackMoveDirection::Down);

    // [THEN] Track positions are swapped
    EXPECT_EQ(TrackPosition(trackId1), 1) << "Track 1 is not at position 1";
    EXPECT_EQ(TrackPosition(trackId2), 0) << "Track 2 is not at position 0";

    // Cleanup
    removeTrack(trackId1);
    removeTrack(trackId2);
}

TEST_F(Au3TracksInteractionTests, MoveTopTrackUpNothingHappens)
{
    // [GIVEN] There is a project with two tracks
    const TrackId trackId1 = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId1, INVALID_TRACK) << "Failed to create track";

    const TrackId trackId2 = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId2, INVALID_TRACK) << "Failed to create track 2";

    // [THEN] Track positions are as expected (track1 = 0, track2 = 1)
    ASSERT_EQ(TrackPosition(trackId1), 0) << "Track 1 is not at position 0";
    ASSERT_EQ(TrackPosition(trackId2), 1) << "Track 2 is not at position 1";

    //! [EXPECT] Notify about track moved
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackMoved(_, _)).Times(0);

    // [WHEN] Moving track1 up
    m_tracksInteraction->moveTracks({ trackId1 }, TrackMoveDirection::Up);

    // [THEN] Track positions are the same
    EXPECT_EQ(TrackPosition(trackId1), 0) << "Track 1 is not at position 0";
    EXPECT_EQ(TrackPosition(trackId2), 1) << "Track 2 is not at position 1";

    // Cleanup
    removeTrack(trackId1);
    removeTrack(trackId2);
}

TEST_F(Au3TracksInteractionTests, MoveBottomTrackDownNothingHappens)
{
    // [GIVEN] There is a project with two tracks
    const TrackId trackId1 = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId1, INVALID_TRACK) << "Failed to create track";

    const TrackId trackId2 = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId2, INVALID_TRACK) << "Failed to create track 2";

    // [THEN] Track positions are as expected (track1 = 0, track2 = 1)
    ASSERT_EQ(TrackPosition(trackId1), 0) << "Track 1 is not at position 0";
    ASSERT_EQ(TrackPosition(trackId2), 1) << "Track 2 is not at position 1";

    //! [EXPECT] Notify about track moved
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackMoved(_, _)).Times(0);

    // [WHEN] Moving track2 down
    m_tracksInteraction->moveTracks({ trackId2 }, TrackMoveDirection::Down);

    // [THEN] Track positions are the same
    EXPECT_EQ(TrackPosition(trackId1), 0) << "Track 1 is not at position 0";
    EXPECT_EQ(TrackPosition(trackId2), 1) << "Track 2 is not at position 1";

    // Cleanup
    removeTrack(trackId1);
    removeTrack(trackId2);
}

TEST_F(Au3TracksInteractionTests, MoveTwoTracksUp)
{
    // [GIVEN] There is a project with three tracks
    const TrackId trackId1 = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId1, INVALID_TRACK) << "Failed to create track";

    const TrackId trackId2 = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId2, INVALID_TRACK) << "Failed to create track 2";

    const TrackId trackId3 = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackId3, INVALID_TRACK) << "Failed to create track 3";

    // [THEN] Track positions are as expected (track1 = 0, track2 = 1, track3 = 2)
    ASSERT_EQ(TrackPosition(trackId1), 0) << "Track 1 is not at position 0";
    ASSERT_EQ(TrackPosition(trackId2), 1) << "Track 2 is not at position 1";
    ASSERT_EQ(TrackPosition(trackId3), 2) << "Track 3 is not at position 2";

    //! [EXPECT] Notify about track moved
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackMoved(_, 0)).Times(1);
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackMoved(_, 1)).Times(1);

    // [WHEN] Moving track2 and track3 up
    m_tracksInteraction->moveTracks({ trackId2, trackId3 }, TrackMoveDirection::Up);

    // [THEN] Track positions are as expected (track1 = 2, track2 = 0, track3 = 1)
    EXPECT_EQ(TrackPosition(trackId1), 2) << "Track 1 is not at position 0";
    EXPECT_EQ(TrackPosition(trackId2), 0) << "Track 2 is not at position 1";
    EXPECT_EQ(TrackPosition(trackId3), 1) << "Track 3 is not at position 2";

    // Cleanup
    removeTrack(trackId1);
    removeTrack(trackId2);
    removeTrack(trackId3);
}

TEST_F(Au3TracksInteractionTests, MoveTwoTracksDown)
{
    // [GIVEN] There is a project with three tracks
    const TrackId trackId1 = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId1, INVALID_TRACK) << "Failed to create track";

    const TrackId trackId2 = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId2, INVALID_TRACK) << "Failed to create track 2";

    const TrackId trackId3 = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackId3, INVALID_TRACK) << "Failed to create track 3";

    // [THEN] Track positions are as expected (track1 = 0, track2 = 1, track3 = 2)
    ASSERT_EQ(TrackPosition(trackId1), 0) << "Track 1 is not at position 0";
    ASSERT_EQ(TrackPosition(trackId2), 1) << "Track 2 is not at position 1";
    ASSERT_EQ(TrackPosition(trackId3), 2) << "Track 3 is not at position 2";

    //! [EXPECT] Notify about track moved
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackMoved(_, 1)).Times(1);
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackMoved(_, 2)).Times(1);

    // [WHEN] Moving track1 and track2 down
    m_tracksInteraction->moveTracks({ trackId1, trackId2 }, TrackMoveDirection::Down);

    // [THEN] Track positions are as expected (track1 = 1, track2 = 2, track3 = 0)
    EXPECT_EQ(TrackPosition(trackId1), 1) << "Track 1 is not at position 0";
    EXPECT_EQ(TrackPosition(trackId2), 2) << "Track 2 is not at position 1";
    EXPECT_EQ(TrackPosition(trackId3), 0) << "Track 3 is not at position 2";

    // Cleanup
    removeTrack(trackId1);
    removeTrack(trackId2);
    removeTrack(trackId3);
}

TEST_F(Au3TracksInteractionTests, MoveTwoTracksToZeroIndex)
{
    // [GIVEN] There is a project with three tracks
    const TrackId trackId1 = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId1, INVALID_TRACK) << "Failed to create track";

    const TrackId trackId2 = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId2, INVALID_TRACK) << "Failed to create track 2";

    const TrackId trackId3 = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackId3, INVALID_TRACK) << "Failed to create track 3";

    // [THEN] Track positions are as expected (track1 = 0, track2 = 1, track3 = 2)
    ASSERT_EQ(TrackPosition(trackId1), 0) << "Track 1 is not at position 0";
    ASSERT_EQ(TrackPosition(trackId2), 1) << "Track 2 is not at position 1";
    ASSERT_EQ(TrackPosition(trackId3), 2) << "Track 3 is not at position 2";

    // [WHEN] Moving track1 and track2 down
    m_tracksInteraction->moveTracksTo({ trackId2, trackId3 }, 0);

    // [THEN] Track positions are as expected (track1 = 1, track2 = 2, track3 = 0)
    EXPECT_EQ(TrackPosition(trackId1), 2) << "Track 1 is not at position 0";
    EXPECT_EQ(TrackPosition(trackId2), 0) << "Track 2 is not at position 1";
    EXPECT_EQ(TrackPosition(trackId3), 1) << "Track 3 is not at position 2";

    // Cleanup
    removeTrack(trackId1);
    removeTrack(trackId2);
    removeTrack(trackId3);
}

TEST_F(Au3TracksInteractionTests, MoveTrackToSameIndexDoNothing)
{
    // [GIVEN] There is a project with two tracks
    const TrackId trackId1 = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId1, INVALID_TRACK) << "Failed to create track";

    const TrackId trackId2 = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId2, INVALID_TRACK) << "Failed to create track 2";

    // [THEN] Track positions are as expected (track1 = 0, track2 = 1)
    ASSERT_EQ(TrackPosition(trackId1), 0) << "Track 1 is not at position 0";
    ASSERT_EQ(TrackPosition(trackId2), 1) << "Track 2 is not at position 1";

    //! [EXPECT] Notify about track moved
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackMoved(_, _)).Times(0);

    // [WHEN] Moving track2 to the same index
    m_tracksInteraction->moveTracksTo({ trackId2 }, 1);

    // [THEN] Track positions are the same
    EXPECT_EQ(TrackPosition(trackId1), 0) << "Track 1 is not at position 0";
    EXPECT_EQ(TrackPosition(trackId2), 1) << "Track 2 is not at position 1";

    // Cleanup
    removeTrack(trackId1);
    removeTrack(trackId2);
}

TEST_F(Au3TracksInteractionTests, InsertSilenceWithEmptyTrackCreatesNewTrack)
{
    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackAdded(_)).Times(1);

    //! [WHEN] Insert silence on the empty track
    const secs_t begin = 0;
    const secs_t end = 2 * SAMPLE_INTERVAL;
    const secs_t duration = end - begin;
    m_tracksInteraction->insertSilence({}, begin, end, duration);

    //! [THEN] The project has a new track with a single clip
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 1) << "The number of tracks after the insert silence operation is not 1";

    const TrackId newTrackId = (*projectTracks.begin())->GetId();
    Au3WaveTrack* newTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(newTrackId));
    ASSERT_EQ(newTrack->NIntervals(), 1) << "The number of intervals after the insert silence operation is not 1";

    const WaveTrack::IntervalConstHolder newClip = newTrack->GetSortedClipByIndex(0);
    ASSERT_NE(newClip, nullptr) << "The new clip is not found";
    ValidateClipProperties(newClip, begin, end, begin, end);

    // Cleanup
    removeTrack(newTrackId);
}

TEST_F(Au3TracksInteractionTests, InsertSilenceOnEmptySpace)
{
    //! [GIVEN] There is a project with a track and two clips
    const TrackId trackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(1);

    // [WHEN] Insert silence on the empty space
    const secs_t begin = TRACK_TWO_CLIPS_CLIP2_END + 2 * SAMPLE_INTERVAL;
    const secs_t end = TRACK_TWO_CLIPS_CLIP2_END + 4 * SAMPLE_INTERVAL;
    const secs_t duration = end - begin;
    m_tracksInteraction->insertSilence({ trackId }, begin, end, duration);

    // [THEN] Track now has 3 clips
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    ASSERT_EQ(track->NIntervals(), 3) << "The number of intervals after the insert silence operation is not 3";

    const WaveTrack::IntervalConstHolder newClip = track->GetSortedClipByIndex(2);
    ASSERT_NE(newClip, nullptr) << "The new clip is not found";
    ValidateClipProperties(newClip, begin, end);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3TracksInteractionTests, PasteEmptyDataReturnsError)
{
    constexpr auto moveClips = true;
    constexpr auto moveAllTracks = true;
    constexpr auto isMultiSelectionCopy = false;
    bool projectWasModified = false;
    const muse::Ret ret = m_tracksInteraction->paste({}, 0.0, moveClips, moveAllTracks, isMultiSelectionCopy, projectWasModified);
    ASSERT_EQ(ret, make_ret(Err::TrackEmpty)) << "The return value is not TrackEmpty";
}

TEST_F(Au3TracksInteractionTests, PasteOnEmptyTrack)
{
    //! [GIVEN] There is a project with one track and two clips
    const TrackId trackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";
    const Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);

    //! [GIVEN] Clipboard contains data
    const Au3Track::Holder trackCopy = track->Copy(firstClip->GetSequenceStartTime(), firstClip->GetSequenceEndTime());
    ASSERT_NE(trackCopy, nullptr) << "Failed to copy clip";
    const ITrackDataPtr trackData = std::make_shared<Au3TrackData>(trackCopy);

    //! [EXPECT] The playback is asked for its position
    EXPECT_CALL(*m_playbackState, playbackPosition()).Times(1).WillOnce(Return(0.0));

    //! [WHEN] Paste from clipboard
    constexpr auto moveClips = true;
    constexpr auto moveAllTracks = true;
    constexpr auto isMultiSelectionCopy = false;
    auto projectWasModified = false;
    const muse::Ret ret
        = m_tracksInteraction->paste({ trackData }, 0.0, moveClips, moveAllTracks, isMultiSelectionCopy, projectWasModified);
    ASSERT_EQ(ret, muse::make_ok()) << "The return value is not Ok";

    //! [THEN] The project has a new track with a single clip
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 2) << "The number of tracks after the paste operation is not 2";

    const TrackId newTrackId = (*projectTracks.rbegin())->GetId();
    Au3WaveTrack* newTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(newTrackId));
    ASSERT_EQ(newTrack->NIntervals(), 1) << "The number of intervals after the paste operation is not 1";

    // Cleanup
    removeTrack(trackId);
    removeTrack(newTrackId);
}

TEST_F(Au3TracksInteractionTests, PasteLabelsDistributesToMultipleTracks)
{
    //! [GIVEN] There is a project with two label tracks, each containing a label
    TrackTemplateFactory factory(projectRef(), DEFAULT_SAMPLE_RATE);

    const TrackId sourceLabelTrack1Id = factory.addLabelTrackFromTemplate("Source Label Track 1", {
            { 1.0, 2.0, "Label 1" }
        });

    const TrackId waveTrackId = factory.addTrackFromTemplate("Wave Track", {
            { 0.0, { { 1.0, TrackTemplateFactory::createNoise } } }
        });

    const TrackId sourceLabelTrack2Id = factory.addLabelTrackFromTemplate("Source Label Track 2", {
            { 3.0, 4.0, "Label 2" }
        });

    Au3LabelTrack* sourceLabelTrack1 = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(sourceLabelTrack1Id));
    ASSERT_NE(sourceLabelTrack1, nullptr) << "Failed to find first source label track";
    Au3LabelTrack* sourceLabelTrack2 = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(sourceLabelTrack2Id));
    ASSERT_NE(sourceLabelTrack2, nullptr) << "Failed to find second source label track";

    //! [GIVEN] Clipboard contains data
    const Au3Track::Holder trackCopy1 = sourceLabelTrack1->Copy(0.0, 10.0);
    ASSERT_NE(trackCopy1, nullptr) << "Failed to copy first label track";
    const ITrackDataPtr trackData1 = std::make_shared<Au3TrackData>(trackCopy1);

    const Au3Track::Holder trackCopy2 = sourceLabelTrack2->Copy(0.0, 10.0);
    ASSERT_NE(trackCopy2, nullptr) << "Failed to copy second label track";
    const ITrackDataPtr trackData2 = std::make_shared<Au3TrackData>(trackCopy2);

    //! [GIVEN] Create two destination label tracks and select them
    const TrackId dstLabelTrack1Id = factory.addLabelTrackFromTemplate("Destination Label Track 1", {});

    //! [GIVEN] Add a wave track between destination label tracks
    const TrackId dstWaveTrackId = factory.addTrackFromTemplate("Destination Wave Track", {
            { 0.0, { { 1.0, TrackTemplateFactory::createNoise } } }
        });

    const TrackId dstLabelTrack2Id = factory.addLabelTrackFromTemplate("Destination Label Track 2", {});
    ON_CALL(*m_selectionController, selectedTracks())
    .WillByDefault(Return(TrackIdList { dstLabelTrack1Id, dstLabelTrack2Id }));

    //! [WHEN] Paste from clipboard
    constexpr auto moveClips = false;
    constexpr auto moveAllTracks = false;
    constexpr auto isMultiSelectionCopy = false;
    auto projectWasModified = false;
    const muse::Ret ret = m_tracksInteraction->paste({ trackData1, trackData2 }, 5.0, moveClips,
                                                     moveAllTracks, isMultiSelectionCopy, projectWasModified);
    ASSERT_EQ(ret, muse::make_ok()) << "The return value is not Ok";

    //! [THEN] Labels are distributed to different tracks
    Au3LabelTrack* dstLabelTrack1 = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(dstLabelTrack1Id));
    ASSERT_NE(dstLabelTrack1, nullptr) << "Failed to find first destination label track";
    Au3LabelTrack* dstLabelTrack2 = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(dstLabelTrack2Id));
    ASSERT_NE(dstLabelTrack2, nullptr) << "Failed to find second destination label track";

    ASSERT_EQ(dstLabelTrack1->GetNumLabels(), 1) << "First destination track should have 1 label";
    ASSERT_EQ(dstLabelTrack2->GetNumLabels(), 1) << "Second destination track should have 1 label";

    //! [THEN] Labels are pasted at the correct position
    const Au3Label* label1 = dstLabelTrack1->GetLabel(0);
    ASSERT_NE(label1, nullptr) << "First label should exist";
    ASSERT_DOUBLE_EQ(label1->getT0(), 6.0) << "First label start time should be 6.0 (5.0 + 1.0)";
    ASSERT_DOUBLE_EQ(label1->getT1(), 7.0) << "First label end time should be 7.0 (5.0 + 2.0)";

    const Au3Label* label2 = dstLabelTrack2->GetLabel(0);
    ASSERT_NE(label2, nullptr) << "Second label should exist";
    ASSERT_DOUBLE_EQ(label2->getT0(), 8.0) << "Second label start time should be 8.0 (5.0 + 3.0)";
    ASSERT_DOUBLE_EQ(label2->getT1(), 9.0) << "Second label end time should be 9.0 (5.0 + 4.0)";

    // Cleanup
    removeTrack(sourceLabelTrack1Id);
    removeTrack(waveTrackId);
    removeTrack(sourceLabelTrack2Id);
    removeTrack(dstLabelTrack1Id);
    removeTrack(dstWaveTrackId);
    removeTrack(dstLabelTrack2Id);
}

TEST_F(Au3TracksInteractionTests, PasteLabelTrackWhenWaveTrackSelected)
{
    //! [GIVEN] There is a project with tracks: Wave, Label, Wave, Label
    TrackTemplateFactory factory(projectRef(), DEFAULT_SAMPLE_RATE);

    const TrackId waveTrack1Id = factory.addTrackFromTemplate("Wave Track 1", {
            { 0.0, { { 1.0, TrackTemplateFactory::createNoise } } }
        });

    const TrackId sourceLabelTrackId = factory.addLabelTrackFromTemplate("Source Label Track", {
            { 1.0, 2.0, "Label to copy" }
        });

    const TrackId waveTrack2Id = factory.addTrackFromTemplate("Wave Track 2", {
            { 0.0, { { 1.0, TrackTemplateFactory::createNoise } } }
        });

    const TrackId dstLabelTrackId = factory.addLabelTrackFromTemplate("Destination Label Track", {});

    //! [GIVEN] Clipboard contains label data from the first label track
    Au3LabelTrack* sourceLabelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(sourceLabelTrackId));
    ASSERT_NE(sourceLabelTrack, nullptr) << "Failed to find source label track";
    const Au3Track::Holder trackCopy = sourceLabelTrack->Copy(0.0, 10.0);
    ASSERT_NE(trackCopy, nullptr) << "Failed to copy label track";
    const ITrackDataPtr trackData = std::make_shared<Au3TrackData>(trackCopy);

    //! [GIVEN] Select the second wave track
    ON_CALL(*m_selectionController, selectedTracks())
    .WillByDefault(Return(TrackIdList { waveTrack2Id }));

    //! [WHEN] Paste from clipboard
    constexpr auto moveClips = false;
    constexpr auto moveAllTracks = false;
    constexpr auto isMultiSelectionCopy = false;
    auto projectWasModified = false;
    const muse::Ret ret = m_tracksInteraction->paste({ trackData }, 5.0, moveClips,
                                                     moveAllTracks, isMultiSelectionCopy, projectWasModified);
    ASSERT_EQ(ret, muse::make_ok()) << "The return value is not Ok";

    //! [THEN] Label is pasted into the next label track after the selected wave track
    Au3LabelTrack* dstLabelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(dstLabelTrackId));
    ASSERT_NE(dstLabelTrack, nullptr) << "Failed to find destination label track";
    ASSERT_EQ(dstLabelTrack->GetNumLabels(), 1) << "Destination track should have 1 label";

    //! [THEN] Label is pasted at the correct position
    const Au3Label* label = dstLabelTrack->GetLabel(0);
    ASSERT_NE(label, nullptr) << "Label should exist";
    ASSERT_DOUBLE_EQ(label->getT0(), 6.0) << "Label start time should be 6.0 (5.0 + 1.0)";
    ASSERT_DOUBLE_EQ(label->getT1(), 7.0) << "Label end time should be 7.0 (5.0 + 2.0)";

    // Cleanup
    removeTrack(waveTrack1Id);
    removeTrack(sourceLabelTrackId);
    removeTrack(waveTrack2Id);
    removeTrack(dstLabelTrackId);
}

TEST_F(Au3TracksInteractionTests, PasteLabelTrackCreatesNewTrack)
{
    //! [GIVEN] There is a project with tracks: Wave, Label, Wave (no label tracks after the selected wave)
    TrackTemplateFactory factory(projectRef(), DEFAULT_SAMPLE_RATE);

    const TrackId waveTrack1Id = factory.addTrackFromTemplate("Wave Track 1", {
            { 0.0, { { 1.0, TrackTemplateFactory::createNoise } } }
        });

    const TrackId sourceLabelTrackId = factory.addLabelTrackFromTemplate("Source Label Track", {
            { 1.0, 2.0, "Label to copy" }
        });

    const TrackId waveTrack2Id = factory.addTrackFromTemplate("Wave Track 2", {
            { 0.0, { { 1.0, TrackTemplateFactory::createNoise } } }
        });

    //! [GIVEN] Clipboard contains label data from the label track
    Au3LabelTrack* sourceLabelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(sourceLabelTrackId));
    ASSERT_NE(sourceLabelTrack, nullptr) << "Failed to find source label track";
    const Au3Track::Holder trackCopy = sourceLabelTrack->Copy(0.0, 10.0);
    ASSERT_NE(trackCopy, nullptr) << "Failed to copy label track";
    const ITrackDataPtr trackData = std::make_shared<Au3TrackData>(trackCopy);

    //! [GIVEN] Select the second wave track
    ON_CALL(*m_selectionController, selectedTracks())
    .WillByDefault(Return(TrackIdList { waveTrack2Id }));

    //! [GIVEN] Count tracks before paste
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    const size_t tracksBeforePaste = projectTracks.Size();
    ASSERT_EQ(tracksBeforePaste, 3) << "Should have 3 tracks before paste";

    //! [EXPECT] The playback is asked for its position (when creating new track)
    EXPECT_CALL(*m_playbackState, playbackPosition()).Times(1).WillOnce(Return(5.0));

    //! [EXPECT] The project is notified about track added
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackAdded(_)).Times(1);

    //! [WHEN] Paste from clipboard
    constexpr auto moveClips = false;
    constexpr auto moveAllTracks = false;
    constexpr auto isMultiSelectionCopy = false;
    auto projectWasModified = false;
    const muse::Ret ret = m_tracksInteraction->paste({ trackData }, 5.0, moveClips,
                                                     moveAllTracks, isMultiSelectionCopy, projectWasModified);
    ASSERT_EQ(ret, muse::make_ok()) << "The return value is not Ok";

    //! [THEN] A new label track was created
    ASSERT_EQ(projectTracks.Size(), tracksBeforePaste + 1) << "Should have one more track after paste";

    //! [THEN] Find the newly created label track (it should be the last track)
    const TrackId newLabelTrackId = (*projectTracks.rbegin())->GetId();
    Au3LabelTrack* newLabelTrack = DomAccessor::findLabelTrack(projectRef(), Au3TrackId(newLabelTrackId));
    ASSERT_NE(newLabelTrack, nullptr) << "Failed to find newly created label track";
    ASSERT_EQ(newLabelTrack->GetNumLabels(), 1) << "New label track should have 1 label";

    //! [THEN] Label is pasted at the correct position
    const Au3Label* label = newLabelTrack->GetLabel(0);
    ASSERT_NE(label, nullptr) << "Label should exist";
    ASSERT_DOUBLE_EQ(label->getT0(), 6.0) << "Label start time should be 6.0 (5.0 + 1.0)";
    ASSERT_DOUBLE_EQ(label->getT1(), 7.0) << "Label end time should be 7.0 (5.0 + 2.0)";

    // Cleanup
    removeTrack(waveTrack1Id);
    removeTrack(sourceLabelTrackId);
    removeTrack(waveTrack2Id);
    removeTrack(newLabelTrackId);
}

TEST_F(Au3TracksInteractionTests, AddNewMonoTrack)
{
    //! [GIVEN] There is a project with no tracks
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 0) << "The number of tracks before the add new mono track operation is not 0";

    //! [EXPECT] The project is notified about track added
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackAdded(_)).Times(1);

    //! [EXPECT] The new track is selected
    EXPECT_CALL(*m_selectionController, setSelectedTracks(_, true)).Times(1);

    //! [WHEN] Add a new mono track
    m_tracksInteraction->newMonoTrack();

    //! [THEN] The project has a new track
    ASSERT_EQ(projectTracks.Size(), 1) << "The number of tracks after the add new mono track operation is not 1";

    //! [THEN] The new track is mono
    const auto newTrackId = (*projectTracks.begin())->GetId();
    Au3WaveTrack* newTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(newTrackId));
    ASSERT_EQ(newTrack->NChannels(), 1) << "The channel count of the new track is not 1";

    // Cleanup
    removeTrack(newTrackId);
}

TEST_F(Au3TracksInteractionTests, AddNewStereoTrack)
{
    //! [GIVEN] There is a project with no tracks
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 0) << "The number of tracks before the add new stereo track operation is not 0";

    //! [EXPECT] The project is notified about track added
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackAdded(_)).Times(1);

    //! [EXPECT] The new track is selected
    EXPECT_CALL(*m_selectionController, setSelectedTracks(_, true)).Times(1);

    //! [WHEN] Add a new stereo track
    m_tracksInteraction->newStereoTrack();

    //! [THEN] The project has a new track
    ASSERT_EQ(projectTracks.Size(), 1) << "The number of tracks after the add new stereo track operation is not 1";

    //! [THEN] The new track has two channels
    const auto newTrackId = (*projectTracks.begin())->GetId();
    Au3WaveTrack* newTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(newTrackId));
    ASSERT_EQ(newTrack->NChannels(), 2) << "The channel count of the new track is not 2";

    // Cleanup
    removeTrack(newTrackId);
}

TEST_F(Au3TracksInteractionTests, changeTrackFormat)
{
    //! [GIVEN] There is a project with a track and a single clip
    const TrackId trackId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    ASSERT_EQ(track->NChannels(), 1) << "The channel count of the new track is not 1";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(2);

    //! [WHEN] Change the track format to 16-bit pcm
    const bool ret = m_tracksInteraction->changeTracksFormat({ trackId }, trackedit::TrackFormat::Int16);
    ASSERT_TRUE(ret) << "Failed to change the track format";

    ASSERT_TRUE(track->GetSampleFormat() == sampleFormat::int16Sample) << "The track sample format is not int16";

    //! [WHEN] Change the track format to 24-bit pcm
    const bool ret24 = m_tracksInteraction->changeTracksFormat({ trackId }, trackedit::TrackFormat::Int24);
    ASSERT_TRUE(ret24) << "Failed to change the track format to 24-bit";

    ASSERT_TRUE(track->GetSampleFormat() == sampleFormat::int24Sample) << "The track sample format is not int24";

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3TracksInteractionTests, changeTrackRate)
{
    static constexpr int SAMPLE_RATE = 44100;

    //! [GIVEN] There is a project with a track and a single clip
    const TrackId trackId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    ASSERT_EQ(track->GetRate(), SAMPLE_RATE) << "The sample rate of the new track is not the default";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(1);

    //! [WHEN] Change the track rate to 1 / 10 of the original rate
    const bool ret = m_tracksInteraction->changeTracksRate({ trackId }, SAMPLE_RATE / 10);
    ASSERT_TRUE(ret) << "Failed to change the track rate";

    ASSERT_EQ(track->GetRate(), SAMPLE_RATE / 10) << "The track sample rate is not 4410 Hz";

    //The start time and end time of the clip should be adjusted accordingly
    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    ASSERT_NE(firstClip, nullptr) << "The first clip is not found";
    ValidateClipProperties(firstClip, TRACK_SILENCE_AT_END_CLIP_START, TRACK_SILENCE_AT_END_CLIP_END * 10,
                           TRACK_SILENCE_AT_END_CLIP_START, TRACK_SILENCE_AT_END_CLIP_END * 10);

    // Cleanup
    removeTrack(trackId);
}
}
