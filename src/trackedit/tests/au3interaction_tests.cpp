/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "../internal/au3/au3interaction.h"
#include "../internal/au3/au3trackdata.h"

#include "context/tests/mocks/globalcontextmock.h"
#include "context/tests/mocks/playbackstatemock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "mocks/trackeditprojectmock.h"
#include "mocks/selectioncontrollermock.h"
#include "tracktemplatefactory.h"
#include "../trackediterrors.h"

#include "global/tests/mocks/interactivemock.h"

#include "au3wrap/internal/au3project.h"
#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/trackcolor.h"

using ::testing::NiceMock;
using ::testing::Return;
using ::testing::Truly;
using ::testing::_;

using namespace au;
using namespace au::au3;

namespace au::trackedit {
/*******************************************************************************
 * DEFAULT TRACK CONFIGURATIONS
 * ===========================
 *
 * The test suite uses several track layouts to test data manipulation functions.
 * Below is a visual representation of each track template:
 *
 * TRACK1: Single clip with large silence in the middle
 * -----------------------------------------------------
 * Legend: [A][B] = Audio data, ~~~~ = Silence
 *
 *   |                                                |
 *   |------|                                |--------|
 *   |  A   |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ |   B    |
 *   |------|                                |--------|
 *    10 samples         450 samples          10 samples
 *
 * TRACK2: Single clip with small silence in the middle
 * ----------------------------------------------------
 *
 *   |                       |
 *   |------|------|---------|
 *   |  A   |~~~~~~|    B    |
 *   |------|------|---------|
 *    10 samples 10samples   10 samples
 *
 * TRACK3: Two separate small clips with gap between
 * -------------------------------------------------
 *
 *   |               |          |
 *   |------|        |          |------|
 *   |  A   |        |          |  B   |
 *   |------|        |          |------|
 *   10 samples    10 samples  10 samples
 *              (gap)
 *
 * TRACK4: Single clip with silence at the beginning
 * -------------------------------------------------
 *
 *   |                                    |
 *   |------------------------------------|--------|
 *   |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|    A   |
 *   |------------------------------------|--------|
 *         450 samples                     10 samples
 *
 * TRACK5: Single clip with silence at the end
 * -------------------------------------------
 *
 *   |           |                                     |
 *   |-----------|-------------------------------------|
 *   |     A     |~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|
 *   |-----------|-------------------------------------|
 *    10 samples             450 samples
 *
 * TRACK6: Three clips near each other
 * -----------------------------------
 *
 *  |                                 |
 *  |-----------|-----------|---------|
 *  |     A     |     B     |    C    |
 *  |-----------|-----------|---------|
 *  10 samples   10 samples  10 samples
 *
 ******************************************************************************************/

constexpr static double DEFAULT_SAMPLE_RATE = 44100.0;
constexpr static double SAMPLE_INTERVAL = 1.0 / DEFAULT_SAMPLE_RATE;

constexpr static double TRACK_MIN_SILENCE_CLIP_START = 0.0;
constexpr static double TRACK_MIN_SILENCE_FIRST_SEGMENT_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK_MIN_SILENCE_SILENCE_SEGMENT_DURATION = 450 * SAMPLE_INTERVAL;
constexpr static double TRACK_MIN_SILENCE_SECOND_SEGMENT_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK_MIN_SILENCE_CLIP_DURATION = TRACK_MIN_SILENCE_FIRST_SEGMENT_DURATION
                                                          + TRACK_MIN_SILENCE_SILENCE_SEGMENT_DURATION
                                                          + TRACK_MIN_SILENCE_SECOND_SEGMENT_DURATION;
constexpr static double TRACK_MIN_SILENCE_CLIP_END = TRACK_MIN_SILENCE_CLIP_START + TRACK_MIN_SILENCE_CLIP_DURATION;

constexpr static double TRACK_SMALL_SILENCE_CLIP_START = 0.0;
constexpr static double TRACK_SMALL_SILENCE_FIRST_SEGMENT_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK_SMALL_SILENCE_SILENCE_SEGMENT_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK_SMALL_SILENCE_SECOND_SEGMENT_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK_SMALL_SILENCE_CLIP_DURATION = TRACK_SMALL_SILENCE_FIRST_SEGMENT_DURATION
                                                            + TRACK_SMALL_SILENCE_SILENCE_SEGMENT_DURATION
                                                            + TRACK_SMALL_SILENCE_SECOND_SEGMENT_DURATION;
constexpr static double TRACK_SMALL_SILENCE_CLIP_END = TRACK_SMALL_SILENCE_CLIP_START + TRACK_SMALL_SILENCE_CLIP_DURATION;

constexpr static double TRACK_TWO_CLIPS_CLIP1_START = 0.0;
constexpr static double TRACK_TWO_CLIPS_CLIP1_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK_TWO_CLIPS_CLIP1_END = TRACK_TWO_CLIPS_CLIP1_START + TRACK_TWO_CLIPS_CLIP1_DURATION;
constexpr static double TRACK_TWO_CLIPS_CLIP2_START = 20 * SAMPLE_INTERVAL;
constexpr static double TRACK_TWO_CLIPS_CLIP2_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK_TWO_CLIPS_CLIP2_END = TRACK_TWO_CLIPS_CLIP2_START + TRACK_TWO_CLIPS_CLIP2_DURATION;

constexpr static double TRACK_SILENCE_AT_START_CLIP_START = 0.0;
constexpr static double TRACK_SILENCE_AT_START_SILENCE_DURATION = 450 * SAMPLE_INTERVAL;
constexpr static double TRACK_SILENCE_AT_START_FIRST_SEGMENT_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK_SILENCE_AT_START_CLIP_DURATION = TRACK_SILENCE_AT_START_SILENCE_DURATION
                                                               + TRACK_SILENCE_AT_START_FIRST_SEGMENT_DURATION;
constexpr static double TRACK_SILENCE_AT_START_CLIP_END = TRACK_SILENCE_AT_START_CLIP_START + TRACK_SILENCE_AT_START_CLIP_DURATION;

constexpr static double TRACK_SILENCE_AT_END_CLIP_START = 0.0;
constexpr static double TRACK_SILENCE_AT_END_FIRST_SEGMENT_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK_SILENCE_AT_END_SILENCE_DURATION = 450 * SAMPLE_INTERVAL;
constexpr static double TRACK_SILENCE_AT_END_CLIP_DURATION = TRACK_SILENCE_AT_END_FIRST_SEGMENT_DURATION
                                                             + TRACK_SILENCE_AT_END_SILENCE_DURATION;
constexpr static double TRACK_SILENCE_AT_END_CLIP_END = TRACK_SILENCE_AT_END_CLIP_START + TRACK_SILENCE_AT_END_CLIP_DURATION;

constexpr static double TRACK_THREE_CLIPS_CLIP1_START = 0.0;
constexpr static double TRACK_THREE_CLIPS_CLIP1_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK_THREE_CLIPS_CLIP1_END = TRACK_THREE_CLIPS_CLIP1_START + TRACK_THREE_CLIPS_CLIP1_DURATION;
constexpr static double TRACK_THREE_CLIPS_CLIP2_START = 20 * SAMPLE_INTERVAL;
constexpr static double TRACK_THREE_CLIPS_CLIP2_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK_THREE_CLIPS_CLIP2_END = TRACK_THREE_CLIPS_CLIP2_START + TRACK_THREE_CLIPS_CLIP2_DURATION;
constexpr static double TRACK_THREE_CLIPS_CLIP3_START = 30 * SAMPLE_INTERVAL;
constexpr static double TRACK_THREE_CLIPS_CLIP3_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK_THREE_CLIPS_CLIP3_END = TRACK_THREE_CLIPS_CLIP3_START + TRACK_THREE_CLIPS_CLIP3_DURATION;

enum class TestTrackID : size_t {
    TRACK_MIN_SILENCE = 0,
    TRACK_SMALL_SILENCE,
    TRACK_TWO_CLIPS,
    TRACK_SILENCE_AT_START,
    TRACK_SILENCE_AT_END,
    TRACK_THREE_CLIPS
};

class Au3InteractionTests : public ::testing::Test
{
public:
    void SetUp() override
    {
        m_au3Interaction = std::make_shared<Au3Interaction>();

        m_globalContext = std::make_shared<NiceMock<context::GlobalContextMock> >();
        m_selectionController = std::make_shared<NiceMock<SelectionControllerMock> >();
        m_interactive = std::make_shared<NiceMock<muse::InteractiveMock> >();
        m_playbackState = std::make_shared<NiceMock<context::PlaybackStateMock> >();

        m_au3Interaction->globalContext.set(m_globalContext);
        m_au3Interaction->selectionController.set(m_selectionController);
        m_au3Interaction->interactive.set(m_interactive);

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

    void initTestProject()
    {
        m_au3ProjectAccessor = std::make_shared<au3::Au3ProjectAccessor>();
        const muse::io::path_t TEST_PROJECT_PATH = muse::String::fromUtf8(trackedit_tests_DATA_ROOT) + "/data/empty.aup3";
        muse::Ret ret = m_au3ProjectAccessor->load(TEST_PROJECT_PATH);

        ON_CALL(*m_currentProject, au3ProjectPtr())
        .WillByDefault(Return(m_au3ProjectAccessor->au3ProjectPtr()));
    }

    TrackId createTrack(const TestTrackID TestTrackID)
    {
        TrackTemplateFactory factory(projectRef(), DEFAULT_SAMPLE_RATE);

        TrackId trackId = INVALID_TRACK;
        switch (TestTrackID) {
        case TestTrackID::TRACK_MIN_SILENCE:
            trackId = factory.addTrackFromTemplate("clipWithMinSilence", {
                    { TRACK_MIN_SILENCE_CLIP_START, {
                          { TRACK_MIN_SILENCE_FIRST_SEGMENT_DURATION, TrackTemplateFactory::createNoise },
                          { TRACK_MIN_SILENCE_SILENCE_SEGMENT_DURATION, TrackTemplateFactory::createSilence },
                          { TRACK_MIN_SILENCE_SECOND_SEGMENT_DURATION, TrackTemplateFactory::createNoise }
                      } }
                });
            break;
        case TestTrackID::TRACK_SMALL_SILENCE:
            trackId = factory.addTrackFromTemplate("clipWithSmallSilence", {
                    { TRACK_SMALL_SILENCE_CLIP_START, {
                          { TRACK_SMALL_SILENCE_FIRST_SEGMENT_DURATION, TrackTemplateFactory::createNoise },
                          { TRACK_SMALL_SILENCE_SILENCE_SEGMENT_DURATION, TrackTemplateFactory::createSilence },
                          { TRACK_SMALL_SILENCE_SECOND_SEGMENT_DURATION, TrackTemplateFactory::createNoise }
                      } }
                });
            break;
        case TestTrackID::TRACK_TWO_CLIPS:
            trackId = factory.addTrackFromTemplate("twoClips", {
                    { TRACK_TWO_CLIPS_CLIP1_START, {
                          { TRACK_TWO_CLIPS_CLIP1_DURATION, TrackTemplateFactory::createNoise }
                      } },
                    { TRACK_TWO_CLIPS_CLIP2_START, {
                          { TRACK_TWO_CLIPS_CLIP2_DURATION, TrackTemplateFactory::createNoise }
                      } }
                });
            break;
        case TestTrackID::TRACK_SILENCE_AT_START:
            trackId = factory.addTrackFromTemplate("clipWithSilenceAtStart", {
                    { TRACK_SILENCE_AT_START_CLIP_START, {
                          { TRACK_SILENCE_AT_START_SILENCE_DURATION, TrackTemplateFactory::createSilence },
                          { TRACK_SILENCE_AT_START_FIRST_SEGMENT_DURATION, TrackTemplateFactory::createNoise }
                      } }
                });
            break;
        case TestTrackID::TRACK_SILENCE_AT_END:
            trackId = factory.addTrackFromTemplate("clipWithSilenceAtEnd", {
                    { TRACK_SILENCE_AT_END_CLIP_START, {
                          { TRACK_SILENCE_AT_END_FIRST_SEGMENT_DURATION, TrackTemplateFactory::createNoise },
                          { TRACK_SILENCE_AT_END_SILENCE_DURATION, TrackTemplateFactory::createSilence }
                      } }
                });
            break;
        case TestTrackID::TRACK_THREE_CLIPS:
            trackId = factory.addTrackFromTemplate("threeClips", {
                    { TRACK_THREE_CLIPS_CLIP1_START, {
                          { TRACK_THREE_CLIPS_CLIP1_DURATION, TrackTemplateFactory::createNoise }
                      } },
                    {
                        TRACK_THREE_CLIPS_CLIP2_START, {
                            { TRACK_THREE_CLIPS_CLIP2_DURATION, TrackTemplateFactory::createNoise }
                        }
                    },
                    {
                        TRACK_THREE_CLIPS_CLIP3_START, {
                            { TRACK_THREE_CLIPS_CLIP3_DURATION, TrackTemplateFactory::createNoise }
                        }
                    }
                });
            break;
        default:
            break;
        }

        return trackId;
    }

    void removeTrack(const TrackId trackId)
    {
        Au3TrackList& trackList = Au3TrackList::Get(projectRef());
        Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
        trackList.Remove(*track);
    }

    Au3Project& projectRef() const
    {
        Au3Project* project = reinterpret_cast<Au3Project*>(m_au3ProjectAccessor->au3ProjectPtr());
        return *project;
    }

    void TearDown() override
    {
        Au3TrackList& trackList = Au3TrackList::Get(projectRef());
        trackList.Clear();

        m_au3ProjectAccessor->clearSavedState();
        m_au3ProjectAccessor->close();
    }

    void ValidateClipProperties(const WaveTrack::IntervalHolder& clip, double sequenceStart, double sequenceEnd, double playStart,
                                double playEnd)
    {
        ASSERT_DOUBLE_EQ(clip->GetSequenceStartTime(), sequenceStart) << "Clip sequence start time is not as expected";
        ASSERT_DOUBLE_EQ(clip->GetSequenceEndTime(), sequenceEnd) << "Clip sequence end time is not as expected";
        ASSERT_DOUBLE_EQ(clip->GetPlayStartTime(), playStart) << "Clip play start time is not as expected";
        ASSERT_DOUBLE_EQ(clip->GetPlayEndTime(), playEnd) << "Clip play end time is not as expected";
    }

    void ValidateClipProperties(const WaveTrack::IntervalConstHolder& clip, double sequenceStart, double sequenceEnd, double playStart,
                                double playEnd)
    {
        ASSERT_DOUBLE_EQ(clip->GetSequenceStartTime(), sequenceStart) << "Clip sequence start time is not as expected";
        ASSERT_DOUBLE_EQ(clip->GetSequenceEndTime(), sequenceEnd) << "Clip sequence end time is not as expected";
        ASSERT_DOUBLE_EQ(clip->GetPlayStartTime(), playStart) << "Clip play start time is not as expected";
        ASSERT_DOUBLE_EQ(clip->GetPlayEndTime(), playEnd) << "Clip play end time is not as expected";
    }

    void ValidateClipProperties(const WaveTrack::IntervalHolder& clip, double sequenceStart, double sequenceEnd)
    {
        ValidateClipProperties(clip, sequenceStart, sequenceEnd, sequenceStart, sequenceEnd);
    }

    void ValidateClipProperties(const WaveTrack::IntervalConstHolder& clip, double sequenceStart, double sequenceEnd)
    {
        ValidateClipProperties(clip, sequenceStart, sequenceEnd, sequenceStart, sequenceEnd);
    }

    int TrackPosition(const TrackId trackId)
    {
        return m_au3Interaction->trackPosition(trackId);
    }

    std::shared_ptr<Au3Interaction> m_au3Interaction;

    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<project::AudacityProjectMock> m_currentProject;
    std::shared_ptr<TrackeditProjectMock> m_trackEditProject;
    std::shared_ptr<SelectionControllerMock> m_selectionController;
    std::shared_ptr<muse::IInteractive> m_interactive;
    std::shared_ptr<context::PlaybackStateMock> m_playbackState;

    std::shared_ptr<au3::Au3ProjectAccessor> m_au3ProjectAccessor;
};

TEST_F(Au3InteractionTests, ChangeClipColor)
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
    m_au3Interaction->changeClipColor(ClipKey { au3WaveTrack->GetId(), au3Clip->GetId() }, "red");

    //! [THEN] The color is updated
    const std::shared_ptr<Au3WaveClip> au3UpdatedClip = DomAccessor::findWaveClip(project, au3WaveTrack->GetId(), 0);
    EXPECT_EQ(au3UpdatedClip->GetColor(), "red");

    // Cleanup
    removeTrack(trackMinSilenceId);
}

TEST_F(Au3InteractionTests, ClipColorRetainedWhenClipIsCopied)
{
    //! [GIVEN] There is a project with a track and a clip with a custom color
    const TrackId trackId = createTrack(TestTrackID::TRACK_MIN_SILENCE);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3Project& project = projectRef();
    const Au3WaveTrack* track = DomAccessor::findWaveTrack(project, Au3TrackId(trackId));
    const std::shared_ptr<Au3WaveClip> clip = DomAccessor::findWaveClip(project, track->GetId(), 0);

    m_au3Interaction->changeClipColor(ClipKey { track->GetId(), clip->GetId() }, "red");

    const Au3Track::Holder trackCopy = track->Copy(clip->GetSequenceStartTime(), clip->GetSequenceEndTime());
    ASSERT_NE(trackCopy, nullptr) << "Failed to copy clip";
    const ITrackDataPtr trackData = std::make_shared<Au3TrackData>(trackCopy);

    //! [EXPECT] The playback is asked for its position
    EXPECT_CALL(*m_playbackState, playbackPosition()).Times(1).WillOnce(Return(0.0));

    //! [WHEN] Making a clip copy to a new track
    const ClipKey clipKey { track->GetId(), clip->GetId() };
    constexpr auto moveClips = true;
    constexpr auto moveAllTracks = true;
    constexpr auto isMultiSelectionCopy = false;
    auto projectWasModified = false;
    const muse::Ret ret = m_au3Interaction->paste({ trackData }, 0.0, moveClips, moveAllTracks, isMultiSelectionCopy, projectWasModified);
    const Au3TrackList& projectTracks = Au3TrackList::Get(project);
    const TrackId newTrackId = (*projectTracks.rbegin())->GetId();

    //! [THEN] The color should be retained for the copy
    const std::shared_ptr<Au3WaveClip> clipCopy = DomAccessor::findWaveClip(project, newTrackId, 0.0);
    EXPECT_EQ(clipCopy->GetColor(), "red");

    // Cleanup
    removeTrack(trackId);
    removeTrack(newTrackId);
}

TEST_F(Au3InteractionTests, ChangeTrackColor)
{
    //! [GIVEN] There is a project with a track
    const TrackId trackMinSilenceId = createTrack(TestTrackID::TRACK_MIN_SILENCE);
    ASSERT_NE(trackMinSilenceId, INVALID_TRACK) << "Failed to create track";

    Au3Project& project = projectRef();
    const std::shared_ptr<Au3WaveClip> au3Clip = DomAccessor::findWaveClip(project, trackMinSilenceId, 0);

    m_au3Interaction->changeClipColor(ClipKey { trackMinSilenceId, au3Clip->GetId() }, "#66A3FF");

    //! [THEN] The track edit project should notify about the track and clip change
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(Truly([&](const Track& modifiedTrack) {
        return modifiedTrack.id == trackMinSilenceId;
    }))).Times(1);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(Truly([=](const Clip& clip) {
        return clip.key == ClipKey { trackMinSilenceId, au3Clip->GetId() };
    })));

    //! [WHEN] Change the color of the track
    m_au3Interaction->changeTracksColor({ trackMinSilenceId }, "#48BECF");

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

TEST_F(Au3InteractionTests, SplitRangeSelectionAtSilencesOnValidInterval)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const TrackId trackMinSilenceId = createTrack(TestTrackID::TRACK_MIN_SILENCE);
    ASSERT_NE(trackMinSilenceId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackMinSilenceId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin within the clip bondaries
    m_au3Interaction->splitRangeSelectionAtSilences({ track->GetId() }, TRACK_MIN_SILENCE_CLIP_START, TRACK_MIN_SILENCE_CLIP_END);

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

TEST_F(Au3InteractionTests, SplitRangeSelectionAtSilencesOnInvalidInterval)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const TrackId trackMinSilenceId = createTrack(TestTrackID::TRACK_MIN_SILENCE);
    ASSERT_NE(trackMinSilenceId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackMinSilenceId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin outside the clip bondaries
    m_au3Interaction->splitRangeSelectionAtSilences(
        { track->GetId() }, TRACK_MIN_SILENCE_CLIP_END, TRACK_MIN_SILENCE_CLIP_END + 1.0);

    //! [THEN] The number of intervals is still one
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split range operation is not 1";

    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(firstClip, TRACK_MIN_SILENCE_CLIP_START, TRACK_MIN_SILENCE_CLIP_END);

    // Cleanup
    removeTrack(trackMinSilenceId);
}

TEST_F(Au3InteractionTests, SplitRangeSelectionAtSilencesOnIntervalWithShortSilence)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const TrackId trackSmallSilenceId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackSmallSilenceId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackSmallSilenceId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin the clip with a small silence
    m_au3Interaction->splitRangeSelectionAtSilences({ track->GetId() }, TRACK_SMALL_SILENCE_CLIP_START, TRACK_SMALL_SILENCE_CLIP_END);

    //! [THEN] The number of intervals is 1 once the silence is less than 0.01s
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split range operation is not 1";

    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(firstClip, TRACK_SMALL_SILENCE_CLIP_START, TRACK_SMALL_SILENCE_CLIP_END);

    // Cleanup
    removeTrack(trackSmallSilenceId);
}

TEST_F(Au3InteractionTests, SplitClipsAtSilencesOnValidInterval)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const TrackId trackMinSilenceId = createTrack(TestTrackID::TRACK_MIN_SILENCE);
    ASSERT_NE(trackMinSilenceId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackMinSilenceId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin the clip
    const WaveTrack::IntervalConstHolder clip = track->GetSortedClipByIndex(0);
    m_au3Interaction->splitClipsAtSilences({ { track->GetId(), clip->GetId() } });

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

TEST_F(Au3InteractionTests, SplitClipsAtSilencesOnIntervalWithShortSilence)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const TrackId trackSmallSilenceId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackSmallSilenceId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackSmallSilenceId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin the clip with a small silence
    const WaveTrack::IntervalConstHolder clip = track->GetSortedClipByIndex(0);
    m_au3Interaction->splitClipsAtSilences({ { track->GetId(), clip->GetId() } });

    //! [THEN] The number of intervals is 1 once the silence is less than 0.01s
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split range operation is not 1";

    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(firstClip, TRACK_SMALL_SILENCE_CLIP_START, TRACK_SMALL_SILENCE_CLIP_END);

    // Cleanup
    removeTrack(trackSmallSilenceId);
}

TEST_F(Au3InteractionTests, SplitClipsAtSilenceWhenSilenceAtStart)
{
    //! [GIVEN] There is a project with a track and a clip with silence at the start
    const TrackId trackSilenceAtStartId = createTrack(TestTrackID::TRACK_SILENCE_AT_START);
    ASSERT_NE(trackSilenceAtStartId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackSilenceAtStartId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin the clip
    const WaveTrack::IntervalConstHolder clip = track->GetSortedClipByIndex(0);
    m_au3Interaction->splitClipsAtSilences({ { track->GetId(), clip->GetId() } });

    //! [THEN] The number of intervals is 1 once the silence is at the start
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split range operation is not 1";

    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(firstClip, TRACK_SILENCE_AT_START_CLIP_START, TRACK_SILENCE_AT_START_CLIP_END,
                           TRACK_SILENCE_AT_START_CLIP_START + TRACK_SILENCE_AT_START_SILENCE_DURATION,
                           TRACK_SILENCE_AT_START_CLIP_END);

    // Cleanup
    removeTrack(trackSilenceAtStartId);
}

TEST_F(Au3InteractionTests, SplitClipsAtSilenceWhenSilenceAtEnd)
{
    //! [GIVEN] There is a project with a track and a clip with silence at the end
    const TrackId trackSilenceAtEndId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackSilenceAtEndId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackSilenceAtEndId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin the clip
    const WaveTrack::IntervalConstHolder clip = track->GetSortedClipByIndex(0);
    m_au3Interaction->splitClipsAtSilences({ { track->GetId(), clip->GetId() } });

    //! [THEN] The number of intervals is 1 once the silence is at the end
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split range operation is not 1";

    const WaveTrack::IntervalConstHolder firstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(firstClip, TRACK_SILENCE_AT_START_CLIP_START, TRACK_SILENCE_AT_START_CLIP_END, TRACK_SILENCE_AT_START_CLIP_START,
                           TRACK_SILENCE_AT_START_CLIP_START + TRACK_SILENCE_AT_START_FIRST_SEGMENT_DURATION);

    // Cleanup
    removeTrack(trackSilenceAtEndId);
}

TEST_F(Au3InteractionTests, MergeSelectedOnTrackOnValidInterval)
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
    m_au3Interaction->mergeSelectedOnTracks({ track->GetId() }, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP2_END);

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the merge operation is not 1";

    const WaveTrack::IntervalConstHolder mergedClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(mergedClip, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP2_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, SplitRangeSelectionIntoNewTracks)
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
    m_au3Interaction->splitRangeSelectionIntoNewTracks({ track->GetId() }, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP2_END);

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

TEST_F(Au3InteractionTests, SplitRangeSelectionIntoNewTracksOutOfClipBounds)
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
    m_au3Interaction->splitRangeSelectionIntoNewTracks({ track->GetId() }, TRACK_TWO_CLIPS_CLIP2_END, TRACK_TWO_CLIPS_CLIP2_END + 1.0);

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

TEST_F(Au3InteractionTests, SplitRangeClipsIntoNewTracks)
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
    m_au3Interaction->splitClipsIntoNewTracks({ { track->GetId(), clip1->GetId() } });

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

TEST_F(Au3InteractionTests, RemoveSingleClipFromATrack)
{
    //! [GIVEN] There is a project with a track and a clip
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Remove the clip
    const WaveTrack::IntervalConstHolder clip = track->GetSortedClipByIndex(0);
    m_au3Interaction->removeClip({ track->GetId(), clip->GetId() });

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the remove operation is not 1";

    //! [THEN] The remaining clip keeps the same properties
    const WaveTrack::IntervalConstHolder remainingClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(remainingClip, TRACK_TWO_CLIPS_CLIP2_START, TRACK_TWO_CLIPS_CLIP2_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, RemoveTwoClipsFromATrack)
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
    m_au3Interaction->removeClips({ { track->GetId(), clip1->GetId() }, { track->GetId(), clip2->GetId() } }, false);

    //! [THEN] The number of intervals is 0
    ASSERT_EQ(track->NIntervals(), 0) << "The number of intervals after the remove operation is not 0";

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, RemoveClipsMovingRemaining)
{
    //! [GIVEN] There is a project with a track and two clips
    const TrackId trackThreeClipsId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackThreeClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackThreeClipsId));

    //! [THEN] The number of intervals is 3
    ASSERT_EQ(track->NIntervals(), 3) << "Precondition failed: The number of intervals is not 3";

    //! [WHEN] Remove the second clip
    const WaveTrack::IntervalConstHolder clip = track->GetSortedClipByIndex(1);
    m_au3Interaction->removeClips({ { track->GetId(), clip->GetId() } }, true);

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

TEST_F(Au3InteractionTests, RemoveTracksDataSingleClip)
{
    //! [GIVEN] There is a project with a track and a clip
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Remove the track data
    m_au3Interaction->removeTracksData({ track->GetId() }, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP1_END, false);

    //! [THEN] The number of intervals is 0
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the remove operation is not 1";

    //! [THEN] The remaining clip keeps the same properties
    const WaveTrack::IntervalConstHolder remainingClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(remainingClip, TRACK_TWO_CLIPS_CLIP2_START, TRACK_TWO_CLIPS_CLIP2_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, RemoveTracksDataRipplePerClip)
{
    //! [GIVEN] There is a project with a track and a clip
    const TrackId trackThreeClipsId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackThreeClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackThreeClipsId));

    //! [THEN] The number of intervals is 3
    ASSERT_EQ(track->NIntervals(), 3) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Remove part of the second clip
    m_au3Interaction->removeTracksData({ track->GetId() }, TRACK_THREE_CLIPS_CLIP2_START,
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

TEST_F(Au3InteractionTests, RemoveTracksDataRipplePerTrack)
{
    //! [GIVEN] There is a project with a track and a clip
    const TrackId trackThreeClipsId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackThreeClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackThreeClipsId));

    //! [THEN] The number of intervals is 3
    ASSERT_EQ(track->NIntervals(), 3) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Remove part of the second clip
    m_au3Interaction->removeTracksData({ track->GetId() }, TRACK_THREE_CLIPS_CLIP2_START,
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

TEST_F(Au3InteractionTests, DeleteTracks)
{
    const TrackId trackThreeClipsId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);

    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 2) << "Precondition failed: The number of tracks is not 2";

    //! [WHEN] Delete one track
    m_au3Interaction->deleteTracks({ trackThreeClipsId });

    //! [THEN] The number of tracks is decremented
    ASSERT_EQ(projectTracks.Size(), 1) << "The number of tracks after the delete operation is not 4";

    //Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, CopyClip)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder clip = track->GetSortedClipByIndex(0);
    const ClipKey clipKey { track->GetId(), clip->GetId() };

    //! [WHEN] Copy the tracks
    m_au3Interaction->copyClip(clipKey);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, CopyContinuousTrackData)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));

    //! [WHEN] Copy the tracks inside the clip bounds
    m_au3Interaction->copyContinuousTrackData(track->GetId(), track->GetClip(0)->GetSequenceStartTime(),
                                              track->GetClip(0)->GetSequenceEndTime());

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, CopyContinuousTrackDataOutsideClipBounds)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));

    //! [WHEN] Copy the tracks outside the clip bounds
    m_au3Interaction->copyContinuousTrackData(track->GetId(), track->GetClip(0)->GetSequenceEndTime() + 1.0,
                                              track->GetClip(0)->GetSequenceEndTime() + 2.0);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, CopyContinuousTrackDataThrowsWhenStartIsGreaterThanEnd)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));

    //! [WHEN] Copy the tracks thrown InconsistencyExpection
    ASSERT_THROW(m_au3Interaction->copyContinuousTrackData(track->GetId(), track->GetClip(0)->GetSequenceEndTime(),
                                                           track->GetClip(0)->GetSequenceStartTime()), InconsistencyException);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, CopyNonContinuousTrackData)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));

    //! [WHEN] Copy the tracks
    std::vector<ClipKey> clips;
    for (size_t i = 0; i < track->NIntervals(); i++) {
        clips.push_back({ track->GetId(), track->GetClip(i)->GetId() });
    }
    m_au3Interaction->copyNonContinuousTrackData(track->GetId(), clips, 0);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, CutClip)
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
    m_au3Interaction->cutClip(clipKey);

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "The number of intervals after the cut operation is not 2";

    //! [THEN] The last clip was moved
    const WaveTrack::IntervalConstHolder lastClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(lastClip, middleClipStart, middleClipStart + TRACK_THREE_CLIPS_CLIP3_DURATION);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, CutTrackDataWithoutMovingClips)
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
    m_au3Interaction->cutTrackData(trackId, middleClipStart, midleClipEnd, false);

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "The number of intervals after the cut operation is not 2";

    //! [THEN] The last clip was not moved
    const WaveTrack::IntervalConstHolder modifiedLastClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(modifiedLastClip, lastClipStart, lastClipEnd);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, CutTrackDataMovingClips)
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
    m_au3Interaction->cutTrackData(trackId, middleClipStart, midleClipEnd, true);

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "The number of intervals after the cut operation is not 2";

    //! [THEN] The last clip was moved
    const WaveTrack::IntervalConstHolder modifiedLastClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(modifiedLastClip, middleClipStart, middleClipStart + lastClipDuration);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, DuplicateClipsOnEmptyList)
{
    //! [GIVEN] A project without tracks
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 0) << "Precondition failed: The number of tracks is not 0";

    //! [EXPECT] Notify about track inserted is not called
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackInserted(_, _)).Times(0);

    //! [WHEN] Duplicate the clips with an empty list
    EXPECT_EQ(m_au3Interaction->duplicateClips({}), false);

    //! [THEN] The number of tracks is still 0
    ASSERT_EQ(projectTracks.Size(), 0) << "The number of tracks after the duplicate operation is not 0";
}

TEST_F(Au3InteractionTests, DuplicateSingleClip)
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
    m_au3Interaction->duplicateClip({ track->GetId(), clip->GetId() });

    //! [THEN] The number of tracks is 2
    ASSERT_EQ(projectTracks.Size(), 2) << "The number of tracks after the duplicate operation is not 2";

    //Cleanup
    removeTrack(trackId);
    const TrackId newTrackId = (*projectTracks.begin())->GetId();
    removeTrack(newTrackId);
}

TEST_F(Au3InteractionTests, DuplicateTracksOnEmptyList)
{
    //! [GIVEN] A project without tracks
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 0) << "Precondition failed: The number of tracks is not 0";

    //! [EXPECT] Notify about track inserted is not called
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackInserted(_, _)).Times(0);

    //! [WHEN] Duplicate the tracks with an empty list
    EXPECT_EQ(m_au3Interaction->duplicateTracks({}), true);

    //! [THEN] The number of tracks is still 0
    ASSERT_EQ(projectTracks.Size(), 0) << "The number of tracks after the duplicate operation is not 0";
}

TEST_F(Au3InteractionTests, DuplicateTracks)
{
    const TrackId trackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);

    //! [EXPECT] The number of tracks is 1
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 1) << "Precondition failed: The number of tracks is not 1";

    //! [EXPECT] Notify about track added
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackInserted(_, _)).Times(1);

    //! [WHEN] Duplicate the track
    const TrackIdList trackList { trackId };
    m_au3Interaction->duplicateTracks(trackList);

    //! [THEN] The number of tracks is 2
    ASSERT_EQ(projectTracks.Size(), 2) << "The number of tracks after the duplicate operation is not 2";

    //Cleanup
    removeTrack(trackId);
    const TrackId newTrackId = (*projectTracks.begin())->GetId();
    removeTrack(newTrackId);
}

TEST_F(Au3InteractionTests, DuplicateRangeSelectionOnNewTrack)
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
    m_au3Interaction->duplicateSelectedOnTracks({ track->GetId() }, TRACK_THREE_CLIPS_CLIP2_START, TRACK_THREE_CLIPS_CLIP2_END);

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

TEST_F(Au3InteractionTests, MoveOnEmptyList)
{
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackMoved(_, _)).Times(0);
    m_au3Interaction->moveTracks({}, TrackMoveDirection::Up);
}

TEST_F(Au3InteractionTests, MoveTracksUpSingle)
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
    m_au3Interaction->moveTracks({ trackId2 }, TrackMoveDirection::Up);

    // [THEN] Track positions are swapped
    EXPECT_EQ(TrackPosition(trackId1), 1) << "Track 1 is not at position 1";
    EXPECT_EQ(TrackPosition(trackId2), 0) << "Track 2 is not at position 0";

    // Cleanup
    removeTrack(trackId1);
    removeTrack(trackId2);
}

TEST_F(Au3InteractionTests, MoveTrackDownSingle)
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
    m_au3Interaction->moveTracks({ trackId1 }, TrackMoveDirection::Down);

    // [THEN] Track positions are swapped
    EXPECT_EQ(TrackPosition(trackId1), 1) << "Track 1 is not at position 1";
    EXPECT_EQ(TrackPosition(trackId2), 0) << "Track 2 is not at position 0";

    // Cleanup
    removeTrack(trackId1);
    removeTrack(trackId2);
}

TEST_F(Au3InteractionTests, MoveTopTrackUpNothingHappens)
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
    m_au3Interaction->moveTracks({ trackId1 }, TrackMoveDirection::Up);

    // [THEN] Track positions are the same
    EXPECT_EQ(TrackPosition(trackId1), 0) << "Track 1 is not at position 0";
    EXPECT_EQ(TrackPosition(trackId2), 1) << "Track 2 is not at position 1";

    // Cleanup
    removeTrack(trackId1);
    removeTrack(trackId2);
}

TEST_F(Au3InteractionTests, MoveBottomTrackDownNothingHappens)
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
    m_au3Interaction->moveTracks({ trackId2 }, TrackMoveDirection::Down);

    // [THEN] Track positions are the same
    EXPECT_EQ(TrackPosition(trackId1), 0) << "Track 1 is not at position 0";
    EXPECT_EQ(TrackPosition(trackId2), 1) << "Track 2 is not at position 1";

    // Cleanup
    removeTrack(trackId1);
    removeTrack(trackId2);
}

TEST_F(Au3InteractionTests, MoveTwoTracksUp)
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
    m_au3Interaction->moveTracks({ trackId2, trackId3 }, TrackMoveDirection::Up);

    // [THEN] Track positions are as expected (track1 = 2, track2 = 0, track3 = 1)
    EXPECT_EQ(TrackPosition(trackId1), 2) << "Track 1 is not at position 0";
    EXPECT_EQ(TrackPosition(trackId2), 0) << "Track 2 is not at position 1";
    EXPECT_EQ(TrackPosition(trackId3), 1) << "Track 3 is not at position 2";

    // Cleanup
    removeTrack(trackId1);
    removeTrack(trackId2);
    removeTrack(trackId3);
}

TEST_F(Au3InteractionTests, MoveTwoTracksDown)
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
    m_au3Interaction->moveTracks({ trackId1, trackId2 }, TrackMoveDirection::Down);

    // [THEN] Track positions are as expected (track1 = 1, track2 = 2, track3 = 0)
    EXPECT_EQ(TrackPosition(trackId1), 1) << "Track 1 is not at position 0";
    EXPECT_EQ(TrackPosition(trackId2), 2) << "Track 2 is not at position 1";
    EXPECT_EQ(TrackPosition(trackId3), 0) << "Track 3 is not at position 2";

    // Cleanup
    removeTrack(trackId1);
    removeTrack(trackId2);
    removeTrack(trackId3);
}

TEST_F(Au3InteractionTests, MoveTwoTracksToZeroIndex)
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
    m_au3Interaction->moveTracksTo({ trackId2, trackId3 }, 0);

    // [THEN] Track positions are as expected (track1 = 1, track2 = 2, track3 = 0)
    EXPECT_EQ(TrackPosition(trackId1), 2) << "Track 1 is not at position 0";
    EXPECT_EQ(TrackPosition(trackId2), 0) << "Track 2 is not at position 1";
    EXPECT_EQ(TrackPosition(trackId3), 1) << "Track 3 is not at position 2";

    // Cleanup
    removeTrack(trackId1);
    removeTrack(trackId2);
    removeTrack(trackId3);
}

TEST_F(Au3InteractionTests, MoveTrackToSameIndexDoNothing)
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
    m_au3Interaction->moveTracksTo({ trackId2 }, 1);

    // [THEN] Track positions are the same
    EXPECT_EQ(TrackPosition(trackId1), 0) << "Track 1 is not at position 0";
    EXPECT_EQ(TrackPosition(trackId2), 1) << "Track 2 is not at position 1";

    // Cleanup
    removeTrack(trackId1);
    removeTrack(trackId2);
}

TEST_F(Au3InteractionTests, MoveClipsRight)
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

    EXPECT_CALL(*m_selectionController, selectedClips()).Times(1).WillOnce(Return(ClipKeyList {
            ClipKey { trackId, firstClip->GetId() },
            ClipKey { trackId, middleClip->GetId() },
            ClipKey { trackId, lastClip->GetId() }
        }));
    EXPECT_CALL(*m_selectionController,
                selectedClipsInTrackOrder()).Times(1).WillOnce(Return(ClipKeyList {
            ClipKey { trackId, firstClip->GetId() },
            ClipKey { trackId, middleClip->GetId() },
            ClipKey { trackId, lastClip->GetId() }
        }));

    //! [WHEN] Move the clips right
    auto clipsMovedToOtherTracks = false;
    m_au3Interaction->moveClips(secondsToMove, 0, true, clipsMovedToOtherTracks);

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

TEST_F(Au3InteractionTests, MoveClipLeftWhenClipIsAtZero)
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

    ON_CALL(*m_selectionController,
            selectedClipsInTrackOrder()).WillByDefault(Return(ClipKeyList {
            ClipKey { trackId, firstClip->GetId() },
            ClipKey { trackId, middleClip->GetId() },
            ClipKey { trackId, lastClip->GetId() }
        }));

    ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(ClipKeyList {
            ClipKey { trackId, firstClip->GetId() },
            ClipKey { trackId, middleClip->GetId() },
            ClipKey { trackId, lastClip->GetId() }
        }));

    //! [WHEN] Move the clips left
    auto clipsMovedToOtherTracks = false;
    m_au3Interaction->moveClips(-1.0, 0, true, clipsMovedToOtherTracks);

    //! [THEN] No clip is moved
    const WaveTrack::IntervalConstHolder modifiedFirstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(modifiedFirstClip, firstClipStart, firstClipEnd);
    const WaveTrack::IntervalConstHolder modifiedMiddleClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(modifiedMiddleClip, middleClipStart, middleClipEnd);
    const WaveTrack::IntervalConstHolder modifiedLastClip = track->GetSortedClipByIndex(2);
    ValidateClipProperties(modifiedLastClip, lastClipStart, lastClipEnd);

    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, SplitDeleteOnRangeSelection)
{
    //! [GIVEN] There is a project with a track and a two clips
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";

    const double begin = TRACK_TWO_CLIPS_CLIP1_START + 2 * SAMPLE_INTERVAL;
    const double end = TRACK_TWO_CLIPS_CLIP1_START + 4 * SAMPLE_INTERVAL;

    //! [WHEN] Split and delete part of the first clip
    m_au3Interaction->splitDeleteSelectedOnTracks({ trackTwoClipsId }, begin, end);

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

TEST_F(Au3InteractionTests, SplitDeteleByClipId)
{
    //! [GIVEN] There is a project with a track and two clips
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));
    const WaveTrack::IntervalConstHolder firstClip =  track->GetSortedClipByIndex(0);

    //! [WHEN] Split and delete the whole clip
    m_au3Interaction->clipSplitDelete({ trackTwoClipsId, firstClip->GetId() });

    //! [THEN] Clip was removed there is ony one left
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split delete operation is not 1";

    //! [THEN] The remaining clip stays unchanged
    const WaveTrack::IntervalConstHolder remainingClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(remainingClip, TRACK_TWO_CLIPS_CLIP2_START, TRACK_TWO_CLIPS_CLIP2_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, SplitCutByClipId)
{
    //! [GIVEN] There is a project with a track and two clips
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));
    const WaveTrack::IntervalConstHolder firstClip =  track->GetSortedClipByIndex(0);

    //! [WHEN] Split and cut the whole clip
    m_au3Interaction->clipSplitCut({ trackTwoClipsId, firstClip->GetId() });

    //! [THEN] Clip was removed there is ony one left
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split delete operation is not 1";

    //! [THEN] The remaining clip stays unchanged
    const WaveTrack::IntervalConstHolder remainingClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(remainingClip, TRACK_TWO_CLIPS_CLIP2_START, TRACK_TWO_CLIPS_CLIP2_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, SpliCutOnRangeSelection)
{
    //! [GIVEN] There is a project with a track and two clips
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(1);

    const double begin = TRACK_TWO_CLIPS_CLIP1_START + 2 * SAMPLE_INTERVAL;
    const double end = TRACK_TWO_CLIPS_CLIP1_START + 4 * SAMPLE_INTERVAL;

    //! [WHEN] Split and cut part of the first clip
    m_au3Interaction->splitCutSelectedOnTracks({ trackTwoClipsId }, begin, end);

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

TEST_F(Au3InteractionTests, SplitTracksAtEmptyList)
{
    //! [EXPECT] The project is not notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(0);
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackAdded(_)).Times(0);

    //! [WHEN] Split an empty list
    m_au3Interaction->splitTracksAt({}, { 0.0 });
}

TEST_F(Au3InteractionTests, SplitTracksOnClipData)
{
    //! [GIVEN] There is a project with a track and two clips
    const TrackId trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    //! [WHEN] Split the track in the middle of the first clip
    const secs_t pivot = TRACK_TWO_CLIPS_CLIP1_START + 2 * SAMPLE_INTERVAL;
    m_au3Interaction->splitTracksAt({ trackTwoClipsId }, { pivot });

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

TEST_F(Au3InteractionTests, RangeSplitTracks)
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
    m_au3Interaction->splitTracksAt({ trackTwoClipsId }, { pivots });

    //! [THEN] The clip is splitted in two timepoints
    ASSERT_EQ(track->NIntervals(), 4) << "The number of intervals after the split operation is not 4";

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, TrimSingleClipLeft)
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
    m_au3Interaction->trimClipLeft({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clip is trimmed
    const WaveTrack::IntervalConstHolder trimmedClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(trimmedClip, TRACK_SILENCE_AT_END_CLIP_START, TRACK_SILENCE_AT_END_CLIP_END,
                           TRACK_SILENCE_AT_END_CLIP_START + deltaSec, TRACK_SILENCE_AT_END_CLIP_END);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, TrimSingleClipRight)
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
    m_au3Interaction->trimClipRight({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clip is trimmed
    const WaveTrack::IntervalConstHolder trimmedClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(trimmedClip, TRACK_SILENCE_AT_END_CLIP_START, TRACK_SILENCE_AT_END_CLIP_END, TRACK_SILENCE_AT_END_CLIP_START,
                           TRACK_SILENCE_AT_END_CLIP_END - deltaSec);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, TrimTwoClipsLeftShouldConsiderMinClipDuration)
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
    m_au3Interaction->trimClipLeft({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

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

TEST_F(Au3InteractionTests, TrimTwoClipsRightShouldConsiderMinClipDuration)
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
    m_au3Interaction->trimClipRight({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

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

TEST_F(Au3InteractionTests, StretchSingleClipLeft)
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
    m_au3Interaction->stretchClipLeft({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clip is stretched
    const WaveTrack::IntervalConstHolder stretchedClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(stretchedClip, TRACK_SILENCE_AT_END_CLIP_START + deltaSec, TRACK_SILENCE_AT_END_CLIP_END);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, StretchSingleClipRight)
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
    m_au3Interaction->stretchClipRight({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clip is stretched
    const WaveTrack::IntervalConstHolder stretchedClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(stretchedClip, TRACK_SILENCE_AT_END_CLIP_START, TRACK_SILENCE_AT_END_CLIP_END - deltaSec);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, StretchTwoClipsLeftShouldConsiderMinClipDuration)
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

    m_au3Interaction->stretchClipLeft({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clips are stretched considering the minimum clip duration
    const WaveTrack::IntervalConstHolder stretchedFirstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(stretchedFirstClip, TRACK_TWO_CLIPS_CLIP1_START + minClipDuration, TRACK_TWO_CLIPS_CLIP1_END);

    const WaveTrack::IntervalConstHolder stretchedSecondClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(stretchedSecondClip, TRACK_TWO_CLIPS_CLIP2_START + minClipDuration, TRACK_TWO_CLIPS_CLIP2_END);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, StretchTwoClipsRightShouldConsiderMinClipDuration)
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

    m_au3Interaction->stretchClipRight({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clips are stretched considering the minimum clip duration
    const WaveTrack::IntervalConstHolder stretchedFirstClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(stretchedFirstClip, TRACK_TWO_CLIPS_CLIP1_START, TRACK_TWO_CLIPS_CLIP1_END - minClipDuration);

    const WaveTrack::IntervalConstHolder stretchedSecondClip = track->GetSortedClipByIndex(1);
    ValidateClipProperties(stretchedSecondClip, TRACK_TWO_CLIPS_CLIP2_START, TRACK_TWO_CLIPS_CLIP2_END - minClipDuration);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, InsertSilenceWithEmptyTrackCreatesNewTrack)
{
    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackAdded(_)).Times(1);

    //! [WHEN] Insert silence on the empty track
    const secs_t begin = 0;
    const secs_t end = 2 * SAMPLE_INTERVAL;
    const secs_t duration = end - begin;
    m_au3Interaction->insertSilence({}, begin, end, duration);

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

TEST_F(Au3InteractionTests, InsertSilenceOnEmptySpace)
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
    m_au3Interaction->insertSilence({ trackId }, begin, end, duration);

    // [THEN] Track now has 3 clips
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    ASSERT_EQ(track->NIntervals(), 3) << "The number of intervals after the insert silence operation is not 3";

    const WaveTrack::IntervalConstHolder newClip = track->GetSortedClipByIndex(2);
    ASSERT_NE(newClip, nullptr) << "The new clip is not found";
    ValidateClipProperties(newClip, begin, end);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, PasteEmptyDataReturnsError)
{
    constexpr auto moveClips = true;
    constexpr auto moveAllTracks = true;
    constexpr auto isMultiSelectionCopy = false;
    bool projectWasModified = false;
    const muse::Ret ret = m_au3Interaction->paste({}, 0.0, moveClips, moveAllTracks, isMultiSelectionCopy, projectWasModified);
    ASSERT_EQ(ret, make_ret(Err::TrackEmpty)) << "The return value is not TrackEmpty";
}

TEST_F(Au3InteractionTests, PasteOnEmptyTrack)
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
    const muse::Ret ret = m_au3Interaction->paste({ trackData }, 0.0, moveClips, moveAllTracks, isMultiSelectionCopy, projectWasModified);
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

TEST_F(Au3InteractionTests, AddNewMonoTrack)
{
    //! [GIVEN] There is a project with no tracks
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 0) << "The number of tracks before the add new mono track operation is not 0";

    //! [EXPECT] The project is notified about track added
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackAdded(_)).Times(1);

    //! [EXPECT] The new track is selected
    EXPECT_CALL(*m_selectionController, setSelectedTracks(_, true)).Times(1);

    //! [WHEN] Add a new mono track
    m_au3Interaction->newMonoTrack();

    //! [THEN] The project has a new track
    ASSERT_EQ(projectTracks.Size(), 1) << "The number of tracks after the add new mono track operation is not 1";

    //! [THEN] The new track is mono
    const auto newTrackId = (*projectTracks.begin())->GetId();
    Au3WaveTrack* newTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(newTrackId));
    ASSERT_EQ(newTrack->NChannels(), 1) << "The channel count of the new track is not 1";

    // Cleanup
    removeTrack(newTrackId);
}

TEST_F(Au3InteractionTests, AddNewStereoTrack)
{
    //! [GIVEN] There is a project with no tracks
    const Au3TrackList& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 0) << "The number of tracks before the add new stereo track operation is not 0";

    //! [EXPECT] The project is notified about track added
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackAdded(_)).Times(1);

    //! [EXPECT] The new track is selected
    EXPECT_CALL(*m_selectionController, setSelectedTracks(_, true)).Times(1);

    //! [WHEN] Add a new stereo track
    m_au3Interaction->newStereoTrack();

    //! [THEN] The project has a new track
    ASSERT_EQ(projectTracks.Size(), 1) << "The number of tracks after the add new stereo track operation is not 1";

    //! [THEN] The new track has two channels
    const auto newTrackId = (*projectTracks.begin())->GetId();
    Au3WaveTrack* newTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(newTrackId));
    ASSERT_EQ(newTrack->NChannels(), 2) << "The channel count of the new track is not 2";

    // Cleanup
    removeTrack(newTrackId);
}

TEST_F(Au3InteractionTests, IncreaseClipSpeed)
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
    m_au3Interaction->changeClipSpeed({ trackId, firstClip->GetId() }, speedFactor);

    //! [THEN] The clip change the start and end times
    const WaveTrack::IntervalConstHolder modifiedClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(modifiedClip, TRACK_SILENCE_AT_END_CLIP_START,
                           TRACK_SILENCE_AT_END_CLIP_START + TRACK_SILENCE_AT_END_CLIP_DURATION * speedFactor);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, DecreaseClipSpeed)
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
    m_au3Interaction->changeClipSpeed({ trackId, firstClip->GetId() }, speedFactor);

    //! [THEN] The clip change the start and end times
    const WaveTrack::IntervalConstHolder modifiedClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(modifiedClip, TRACK_SILENCE_AT_END_CLIP_START,
                           TRACK_SILENCE_AT_END_CLIP_START + TRACK_SILENCE_AT_END_CLIP_DURATION * speedFactor);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, ResetClipSpeed)
{
    //! [GIVEN] There is a project with a track and a single clip
    const TrackId trackId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const WaveTrack::IntervalConstHolder firstClip =  track->GetSortedClipByIndex(0);

    //! [GIVEN] The clip speed is not the default
    const double speedFactor = 2.0;
    m_au3Interaction->changeClipSpeed({ trackId, firstClip->GetId() }, speedFactor);

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    //! [WHEN] Reseting the clip speed
    m_au3Interaction->resetClipSpeed({ trackId, firstClip->GetId() });

    //! [THEN] The clip start and end time are the same as the original clip
    const WaveTrack::IntervalConstHolder modifiedClip = track->GetSortedClipByIndex(0);
    ValidateClipProperties(modifiedClip, TRACK_SILENCE_AT_END_CLIP_START, TRACK_SILENCE_AT_END_CLIP_END);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, changeTrackFormat)
{
    //! [GIVEN] There is a project with a track and a single clip
    const TrackId trackId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    ASSERT_EQ(track->NChannels(), 1) << "The channel count of the new track is not 1";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(2);

    //! [WHEN] Change the track format to 16-bit pcm
    const bool ret = m_au3Interaction->changeTracksFormat({ trackId }, trackedit::TrackFormat::Int16);
    ASSERT_TRUE(ret) << "Failed to change the track format";

    ASSERT_TRUE(track->GetSampleFormat() == sampleFormat::int16Sample) << "The track sample format is not int16";

    //! [WHEN] Change the track format to 24-bit pcm
    const bool ret24 = m_au3Interaction->changeTracksFormat({ trackId }, trackedit::TrackFormat::Int24);
    ASSERT_TRUE(ret24) << "Failed to change the track format to 24-bit";

    ASSERT_TRUE(track->GetSampleFormat() == sampleFormat::int24Sample) << "The track sample format is not int24";

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, changeTrackRate)
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
    const bool ret = m_au3Interaction->changeTracksRate({ trackId }, SAMPLE_RATE / 10);
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
