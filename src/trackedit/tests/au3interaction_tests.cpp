/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "../internal/au3/au3interaction.h"

#include "context/tests/mocks/globalcontextmock.h"
#include "context/tests/mocks/playbackstatemock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "mocks/clipboardmock.h"
#include "mocks/trackeditprojectmock.h"
#include "mocks/projecthistorymock.h"
#include "mocks/selectioncontrollermock.h"
#include "tracktemplatefactory.h"
#include "../trackediterrors.h"

#include "global/tests/mocks/interactivemock.h"

#include "au3wrap/internal/au3project.h"
#include "au3wrap/internal/domaccessor.h"

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

constexpr static double TRACK1_CLIP_START = 0.0;
constexpr static double TRACK1_FIRST_SEGMENT_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK1_SILENCE_SEGMENT_DURATION = 450 * SAMPLE_INTERVAL;
constexpr static double TRACK1_SECOND_SEGMENT_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK1_CLIP_DURATION = TRACK1_FIRST_SEGMENT_DURATION + TRACK1_SILENCE_SEGMENT_DURATION
                                               + TRACK1_SECOND_SEGMENT_DURATION;
constexpr static double TRACK1_CLIP_END = TRACK1_CLIP_START + TRACK1_CLIP_DURATION;

constexpr static double TRACK2_CLIP_START = 0.0;
constexpr static double TRACK2_FIRST_SEGMENT_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK2_SILENCE_SEGMENT_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK2_SECOND_SEGMENT_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK2_CLIP_DURATION = TRACK2_FIRST_SEGMENT_DURATION + TRACK2_SILENCE_SEGMENT_DURATION
                                               + TRACK2_SECOND_SEGMENT_DURATION;
constexpr static double TRACK2_CLIP_END = TRACK2_CLIP_START + TRACK2_CLIP_DURATION;

constexpr static double TRACK3_CLIP1_START = 0.0;
constexpr static double TRACK3_CLIP1_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK3_CLIP1_END = TRACK3_CLIP1_START + TRACK3_CLIP1_DURATION;
constexpr static double TRACK3_CLIP2_START = 20 * SAMPLE_INTERVAL;
constexpr static double TRACK3_CLIP2_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK3_CLIP2_END = TRACK3_CLIP2_START + TRACK3_CLIP2_DURATION;

constexpr static double TRACK4_CLIP_START = 0.0;
constexpr static double TRACK4_SILENCE_DURATION = 450 * SAMPLE_INTERVAL;
constexpr static double TRACK4_FIRST_SEGMENT_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK4_CLIP_DURATION = TRACK4_SILENCE_DURATION + TRACK4_FIRST_SEGMENT_DURATION;
constexpr static double TRACK4_CLIP_END = TRACK4_CLIP_START + TRACK4_CLIP_DURATION;

constexpr static double TRACK5_CLIP_START = 0.0;
constexpr static double TRACK5_FIRST_SEGMENT_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK5_SILENCE_DURATION = 450 * SAMPLE_INTERVAL;
constexpr static double TRACK5_CLIP_DURATION = TRACK5_FIRST_SEGMENT_DURATION + TRACK5_SILENCE_DURATION;
constexpr static double TRACK5_CLIP_END = TRACK5_CLIP_START + TRACK5_CLIP_DURATION;

constexpr static double TRACK6_CLIP1_START = 0.0;
constexpr static double TRACK6_CLIP1_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK6_CLIP1_END = TRACK6_CLIP1_START + TRACK6_CLIP1_DURATION;
constexpr static double TRACK6_CLIP2_START = 20 * SAMPLE_INTERVAL;
constexpr static double TRACK6_CLIP2_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK6_CLIP2_END = TRACK6_CLIP2_START + TRACK6_CLIP2_DURATION;
constexpr static double TRACK6_CLIP3_START = 30 * SAMPLE_INTERVAL;
constexpr static double TRACK6_CLIP3_DURATION = 10 * SAMPLE_INTERVAL;
constexpr static double TRACK6_CLIP3_END = TRACK6_CLIP3_START + TRACK6_CLIP3_DURATION;

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
        m_projectHistory = std::make_shared<NiceMock<ProjectHistoryMock> >();
        m_clipboard = std::make_shared<NiceMock<ClipboardMock> >();
        m_selectionController = std::make_shared<NiceMock<SelectionControllerMock> >();
        m_interactive = std::make_shared<NiceMock<muse::InteractiveMock> >();
        m_playbackState = std::make_shared<NiceMock<context::PlaybackStateMock> >();

        m_au3Interaction->globalContext.set(m_globalContext);
        m_au3Interaction->projectHistory.set(m_projectHistory);
        m_au3Interaction->clipboard.set(m_clipboard);
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
                    { TRACK1_CLIP_START, {
                          { TRACK1_FIRST_SEGMENT_DURATION, TrackTemplateFactory::createNoise },
                          { TRACK1_SILENCE_SEGMENT_DURATION, TrackTemplateFactory::createSilence },
                          { TRACK1_SECOND_SEGMENT_DURATION, TrackTemplateFactory::createNoise }
                      } }
                });
            break;
        case TestTrackID::TRACK_SMALL_SILENCE:
            trackId = factory.addTrackFromTemplate("clipWithSmallSilence", {
                    { TRACK2_CLIP_START, {
                          { TRACK2_FIRST_SEGMENT_DURATION, TrackTemplateFactory::createNoise },
                          { TRACK2_SILENCE_SEGMENT_DURATION, TrackTemplateFactory::createSilence },
                          { TRACK2_SECOND_SEGMENT_DURATION, TrackTemplateFactory::createNoise }
                      } }
                });
            break;
        case TestTrackID::TRACK_TWO_CLIPS:
            trackId = factory.addTrackFromTemplate("twoClips", {
                    { TRACK3_CLIP1_START, {
                          { TRACK3_CLIP1_DURATION, TrackTemplateFactory::createNoise }
                      } },
                    { TRACK3_CLIP2_START, {
                          { TRACK3_CLIP2_DURATION, TrackTemplateFactory::createNoise }
                      } }
                });
            break;
        case TestTrackID::TRACK_SILENCE_AT_START:
            trackId = factory.addTrackFromTemplate("clipWithSilenceAtStart", {
                    { TRACK4_CLIP_START, {
                          { TRACK4_SILENCE_DURATION, TrackTemplateFactory::createSilence },
                          { TRACK4_FIRST_SEGMENT_DURATION, TrackTemplateFactory::createNoise }
                      } }
                });
            break;
        case TestTrackID::TRACK_SILENCE_AT_END:
            trackId = factory.addTrackFromTemplate("clipWithSilenceAtEnd", {
                    { TRACK5_CLIP_START, {
                          { TRACK5_FIRST_SEGMENT_DURATION, TrackTemplateFactory::createNoise },
                          { TRACK5_SILENCE_DURATION, TrackTemplateFactory::createSilence }
                      } }
                });
            break;
        case TestTrackID::TRACK_THREE_CLIPS:
            trackId = factory.addTrackFromTemplate("threeClips", {
                    { TRACK6_CLIP1_START, {
                          { TRACK6_CLIP1_DURATION, TrackTemplateFactory::createNoise }
                      } },
                    {
                        TRACK6_CLIP2_START, {
                            { TRACK6_CLIP2_DURATION, TrackTemplateFactory::createNoise }
                        }
                    },
                    {
                        TRACK6_CLIP3_START, {
                            { TRACK6_CLIP3_DURATION, TrackTemplateFactory::createNoise }
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
        auto& trackList = Au3TrackList::Get(projectRef());
        auto track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
        trackList.Remove(*track);
    }

    Au3Project& projectRef() const
    {
        Au3Project* project = reinterpret_cast<Au3Project*>(m_au3ProjectAccessor->au3ProjectPtr());
        return *project;
    }

    void TearDown() override
    {
        auto& trackList = Au3TrackList::Get(projectRef());
        trackList.Clear();

        m_au3ProjectAccessor->clearSavedState();
        m_au3ProjectAccessor->close();
    }

    void ValidateClipProperties(const WaveTrack::IntervalHolder clip, double sequenceStart, double sequenceEnd, double playStart,
                                double playEnd)
    {
        ASSERT_DOUBLE_EQ(clip->GetSequenceStartTime(), sequenceStart) << "Clip sequence start time is not as expected";
        ASSERT_DOUBLE_EQ(clip->GetSequenceEndTime(), sequenceEnd) << "Clip sequence end time is not as expected";
        ASSERT_DOUBLE_EQ(clip->GetPlayStartTime(), playStart) << "Clip play start time is not as expected";
        ASSERT_DOUBLE_EQ(clip->GetPlayEndTime(), playEnd) << "Clip play end time is not as expected";
    }

    int TrackPosition(const TrackId trackId)
    {
        return m_au3Interaction->trackPosition(trackId);
    }

    std::shared_ptr<Au3Interaction> m_au3Interaction;

    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<project::AudacityProjectMock> m_currentProject;
    std::shared_ptr<ClipboardMock> m_clipboard;
    std::shared_ptr<TrackeditProjectMock> m_trackEditProject;
    std::shared_ptr<ProjectHistoryMock> m_projectHistory;
    std::shared_ptr<SelectionControllerMock> m_selectionController;
    std::shared_ptr<muse::IInteractive> m_interactive;
    std::shared_ptr<context::PlaybackStateMock> m_playbackState;

    std::shared_ptr<au3::Au3ProjectAccessor> m_au3ProjectAccessor;
};

TEST_F(Au3InteractionTests, ChangeClipColor)
{
    //! [GIVEN] There is a project with a track and a clip
    const auto trackMinSilenceId = createTrack(TestTrackID::TRACK_MIN_SILENCE);
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

TEST_F(Au3InteractionTests, SplitRangeSelectionAtSilencesOnValidInterval)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const auto trackMinSilenceId = createTrack(TestTrackID::TRACK_MIN_SILENCE);
    ASSERT_NE(trackMinSilenceId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackMinSilenceId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin within the clip bondaries
    m_au3Interaction->splitRangeSelectionAtSilences({ track->GetId() }, TRACK1_CLIP_START, TRACK1_CLIP_END);

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "The number of intervals after the split range operation is not 2";

    auto firstClip = track->GetClip(0);
    ValidateClipProperties(firstClip, TRACK1_CLIP_START, TRACK1_CLIP_END, TRACK1_CLIP_START,
                           TRACK1_CLIP_START + TRACK1_FIRST_SEGMENT_DURATION);

    auto secondClip = track->GetClip(1);
    ValidateClipProperties(secondClip, TRACK1_CLIP_START, TRACK1_CLIP_END,
                           TRACK1_CLIP_START + TRACK1_FIRST_SEGMENT_DURATION + TRACK1_SILENCE_SEGMENT_DURATION,
                           TRACK1_CLIP_END);

    // Cleanup
    removeTrack(trackMinSilenceId);
}

TEST_F(Au3InteractionTests, SplitRangeSelectionAtSilencesOnInvalidInterval)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const auto trackMinSilenceId = createTrack(TestTrackID::TRACK_MIN_SILENCE);
    ASSERT_NE(trackMinSilenceId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackMinSilenceId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin outside the clip bondaries
    m_au3Interaction->splitRangeSelectionAtSilences(
        { track->GetId() }, TRACK1_CLIP_END, TRACK1_CLIP_END + 1.0);

    //! [THEN] The number of intervals is still one
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split range operation is not 1";

    auto firstClip = track->GetClip(0);
    ValidateClipProperties(firstClip, TRACK1_CLIP_START, TRACK1_CLIP_END, TRACK1_CLIP_START, TRACK1_CLIP_END);

    // Cleanup
    removeTrack(trackMinSilenceId);
}

TEST_F(Au3InteractionTests, SplitRangeSelectionAtSilencesOnIntervalWithShortSilence)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const auto trackSmallSilenceId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackSmallSilenceId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackSmallSilenceId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin the clip with a small silence
    m_au3Interaction->splitRangeSelectionAtSilences({ track->GetId() }, TRACK2_CLIP_START, TRACK2_CLIP_END);

    //! [THEN] The number of intervals is 1 once the silence is less than 0.01s
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split range operation is not 1";

    auto firstClip = track->GetClip(0);
    ValidateClipProperties(firstClip, TRACK2_CLIP_START, TRACK2_CLIP_END, TRACK2_CLIP_START, TRACK2_CLIP_END);

    // Cleanup
    removeTrack(trackSmallSilenceId);
}

TEST_F(Au3InteractionTests, SplitClipsAtSilencesOnValidInterval)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const auto trackMinSilenceId = createTrack(TestTrackID::TRACK_MIN_SILENCE);
    ASSERT_NE(trackMinSilenceId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackMinSilenceId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin the clip
    const auto clip = track->GetClip(0);
    m_au3Interaction->splitClipsAtSilences({ { track->GetId(), clip->GetId() } });

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "The number of intervals after the split range operation is not 2";

    auto firstClip = track->GetClip(0);
    ValidateClipProperties(firstClip, TRACK1_CLIP_START, TRACK1_CLIP_END, TRACK1_CLIP_START,
                           TRACK1_CLIP_START + TRACK1_FIRST_SEGMENT_DURATION);

    auto secondClip = track->GetClip(1);
    ValidateClipProperties(secondClip, TRACK1_CLIP_START, TRACK1_CLIP_END,
                           TRACK1_CLIP_START + TRACK1_FIRST_SEGMENT_DURATION + TRACK1_SILENCE_SEGMENT_DURATION,
                           TRACK1_CLIP_END);

    // Cleanup
    removeTrack(trackMinSilenceId);
}

TEST_F(Au3InteractionTests, SplitClipsAtSilencesOnIntervalWithShortSilence)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const auto trackSmallSilenceId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackSmallSilenceId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackSmallSilenceId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin the clip with a small silence
    const auto clip = track->GetClip(0);
    m_au3Interaction->splitClipsAtSilences({ { track->GetId(), clip->GetId() } });

    //! [THEN] The number of intervals is 1 once the silence is less than 0.01s
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split range operation is not 1";

    auto firstClip = track->GetClip(0);
    ValidateClipProperties(firstClip, TRACK2_CLIP_START, TRACK2_CLIP_END, TRACK2_CLIP_START, TRACK2_CLIP_END);

    // Cleanup
    removeTrack(trackSmallSilenceId);
}

TEST_F(Au3InteractionTests, SplitClipsAtSilenceWhenSilenceAtStart)
{
    //! [GIVEN] There is a project with a track and a clip with silence at the start
    const auto trackSilenceAtStartId = createTrack(TestTrackID::TRACK_SILENCE_AT_START);
    ASSERT_NE(trackSilenceAtStartId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackSilenceAtStartId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin the clip
    const auto clip = track->GetClip(0);
    m_au3Interaction->splitClipsAtSilences({ { track->GetId(), clip->GetId() } });

    //! [THEN] The number of intervals is 1 once the silence is at the start
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split range operation is not 1";

    auto firstClip = track->GetClip(0);
    ValidateClipProperties(firstClip, TRACK4_CLIP_START, TRACK4_CLIP_END, TRACK4_CLIP_START + TRACK4_SILENCE_DURATION,
                           TRACK4_CLIP_END);

    // Cleanup
    removeTrack(trackSilenceAtStartId);
}

TEST_F(Au3InteractionTests, SplitClipsAtSilenceWhenSilenceAtEnd)
{
    //! [GIVEN] There is a project with a track and a clip with silence at the end
    const auto trackSilenceAtEndId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackSilenceAtEndId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackSilenceAtEndId));

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "Precondition failed: The number of intervals is not 1";

    //! [WHEN] Disjoin the clip
    const auto clip = track->GetClip(0);
    m_au3Interaction->splitClipsAtSilences({ { track->GetId(), clip->GetId() } });

    //! [THEN] The number of intervals is 1 once the silence is at the end
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split range operation is not 1";

    auto firstClip = track->GetClip(0);
    ValidateClipProperties(firstClip, TRACK4_CLIP_START, TRACK4_CLIP_END, TRACK4_CLIP_START,
                           TRACK4_CLIP_START + TRACK4_FIRST_SEGMENT_DURATION);

    // Cleanup
    removeTrack(trackSilenceAtEndId);
}

TEST_F(Au3InteractionTests, MergeSelectedOnTrackOnValidInterval)
{
    //! [GIVEN] There is a project with a track and two clips
    const auto trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Merge the clips
    const auto clip1 = track->GetClip(0);
    const auto clip2 = track->GetClip(1);
    m_au3Interaction->mergeSelectedOnTracks({ track->GetId() }, TRACK3_CLIP1_START, TRACK3_CLIP2_END);

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the merge operation is not 1";

    auto mergedClip = track->GetClip(0);
    ValidateClipProperties(mergedClip, TRACK3_CLIP1_START, TRACK3_CLIP2_END, TRACK3_CLIP1_START, TRACK3_CLIP2_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, SplitRangeSelectionIntoNewTracks)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const auto trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    //! [THEN] We have one track
    const auto& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 1) << "Precondition failed: The number of tracks is not 1";

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Splitting into new tracks
    m_au3Interaction->splitRangeSelectionIntoNewTracks({ track->GetId() }, TRACK3_CLIP1_START, TRACK3_CLIP2_END);

    //! [THEN] The number of intervals now is 0
    ASSERT_EQ(track->NIntervals(), 0) << "The number of intervals after the split range operation is not 0";

    //! [THEN] Now we have added a new track
    ASSERT_EQ(projectTracks.Size(), 2) << "The number of tracks after the split range operation is not 2";

    //! [THEN] The new track has the same clip information as the original one
    auto newTrack = projectTracks.rbegin();
    auto newTrackId = (*newTrack)->GetId();
    auto newTrackWave = DomAccessor::findWaveTrack(projectRef(), newTrackId);
    ASSERT_EQ(newTrackWave->NIntervals(), 2) << "The number of intervals in the new track is not 2";

    auto firstClip = newTrackWave->GetClip(0);
    ValidateClipProperties(firstClip, TRACK3_CLIP1_START, TRACK3_CLIP1_END, TRACK3_CLIP1_START, TRACK3_CLIP1_END);

    auto secondClip = newTrackWave->GetClip(1);
    ValidateClipProperties(secondClip, TRACK3_CLIP2_START, TRACK3_CLIP2_END, TRACK3_CLIP2_START, TRACK3_CLIP2_END);

    // Cleanup
    removeTrack(newTrackId);
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, SplitRangeSelectionIntoNewTracksOutOfClipBounds)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const auto trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    //! [THEN] We have 1 tracks
    const auto& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 1) << "Precondition failed: The number of tracks is not 1";

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Splitting into new tracks outside the clip bounds
    m_au3Interaction->splitRangeSelectionIntoNewTracks({ track->GetId() }, TRACK3_CLIP2_END, TRACK3_CLIP2_END + 1.0);

    //! [THEN] The number of intervals is still 2
    ASSERT_EQ(track->NIntervals(), 2) << "The number of intervals after the split range operation is not 2";

    //! [THEN] The number of tracks is still one
    ASSERT_EQ(projectTracks.Size(), 1) << "The number of tracks after the split range operation is not 1";

    //! [THEN] The clip information is still the same
    auto firstClip = track->GetClip(0);
    ValidateClipProperties(firstClip, TRACK3_CLIP1_START, TRACK3_CLIP1_END, TRACK3_CLIP1_START, TRACK3_CLIP1_END);

    auto secondClip = track->GetClip(1);
    ValidateClipProperties(secondClip, TRACK3_CLIP2_START, TRACK3_CLIP2_END, TRACK3_CLIP2_START, TRACK3_CLIP2_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, SplitRangeClipsIntoNewTracks)
{
    //! [GIVEN] There is a project with a track and a clip with silence in the middle
    const auto trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    //! [THEN] We have 1 tracks
    const auto& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 1) << "Precondition failed: The number of tracks is not 1";

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Splitting into new tracks
    const auto clip1 = track->GetClip(0);
    const auto clip2 = track->GetClip(1);
    m_au3Interaction->splitClipsIntoNewTracks({ { track->GetId(), clip1->GetId() } });

    //! [THEN] The number of intervals now is 1
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split range operation is not 1";

    //! [THEN] Now we have added a new track
    ASSERT_EQ(projectTracks.Size(),  2) << "The number of tracks after the split range operation is not 2";

    //! [THEN] The original track has only the second clip
    auto firstClip = track->GetClip(0);
    ValidateClipProperties(firstClip, TRACK3_CLIP2_START, TRACK3_CLIP2_END, TRACK3_CLIP2_START, TRACK3_CLIP2_END);

    //! [THEN] The new track has the first clip
    auto newTrack = projectTracks.rbegin();
    auto newTrackId = (*newTrack)->GetId();
    auto newTrackWave = DomAccessor::findWaveTrack(projectRef(), newTrackId);
    ASSERT_EQ(newTrackWave->NIntervals(), 1) << "The number of intervals in the new track is not 1";

    auto newClip = newTrackWave->GetClip(0);
    ValidateClipProperties(newClip, TRACK3_CLIP1_START, TRACK3_CLIP1_END, TRACK3_CLIP1_START, TRACK3_CLIP1_END);

    // Cleanup
    removeTrack(newTrackId);
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, RemoveSingleClipFromATrack)
{
    //! [GIVEN] There is a project with a track and a clip
    const auto trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Remove the clip
    const auto clip = track->GetClip(0);
    m_au3Interaction->removeClip({ track->GetId(), clip->GetId() });

    //! [THEN] The number of intervals is 1
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the remove operation is not 1";

    //! [THEN] The remaining clip keeps the same properties
    const auto remainingClip = track->GetClip(0);
    ValidateClipProperties(remainingClip, TRACK3_CLIP2_START, TRACK3_CLIP2_END, TRACK3_CLIP2_START, TRACK3_CLIP2_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, RemoveTwoClipsFromATrack)
{
    //! [GIVEN] There is a project with a track and two clips
    const auto trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Remove the clips
    const auto clip1 = track->GetClip(0);
    const auto clip2 = track->GetClip(1);
    m_au3Interaction->removeClips({ { track->GetId(), clip1->GetId() }, { track->GetId(), clip2->GetId() } }, false);

    //! [THEN] The number of intervals is 0
    ASSERT_EQ(track->NIntervals(), 0) << "The number of intervals after the remove operation is not 0";

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, RemoveClipsMovingRemaining)
{
    //! [GIVEN] There is a project with a track and two clips
    const auto trackThreeClipsId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackThreeClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackThreeClipsId));

    //! [THEN] The number of intervals is 3
    ASSERT_EQ(track->NIntervals(), 3) << "Precondition failed: The number of intervals is not 3";

    //! [WHEN] Remove the second clip
    const auto clip = track->GetClip(1);
    m_au3Interaction->removeClips({ { track->GetId(), clip->GetId() } }, true);

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "The number of intervals after the remove operation is not 2";

    //! [THEN] The first clip keeps the same properties
    const auto firstClip = track->GetClip(0);
    ValidateClipProperties(firstClip, TRACK6_CLIP1_START, TRACK6_CLIP1_END, TRACK6_CLIP1_START, TRACK6_CLIP1_END);

    //! [THEN] The third clip is moved forward
    const auto thirdClip = track->GetClip(1);
    ValidateClipProperties(thirdClip, TRACK6_CLIP2_START, TRACK6_CLIP2_START + TRACK6_CLIP3_DURATION, TRACK6_CLIP2_START,
                           TRACK6_CLIP2_START + TRACK6_CLIP3_DURATION);

    // Cleanup
    removeTrack(trackThreeClipsId);
}

TEST_F(Au3InteractionTests, RemoveTracksDataSingleClip)
{
    //! [GIVEN] There is a project with a track and a clip
    const auto trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Remove the track data
    m_au3Interaction->removeTracksData({ track->GetId() }, TRACK3_CLIP1_START, TRACK3_CLIP1_END, false);

    //! [THEN] The number of intervals is 0
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the remove operation is not 1";

    //! [THEN] The remaining clip keeps the same properties
    const auto remainingClip = track->GetClip(0);
    ValidateClipProperties(remainingClip, TRACK3_CLIP2_START, TRACK3_CLIP2_END, TRACK3_CLIP2_START, TRACK3_CLIP2_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, RemoveTracksDataRipplePerClip)
{
    //! [GIVEN] There is a project with a track and a clip
    const auto trackThreeClipsId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackThreeClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackThreeClipsId));

    //! [THEN] The number of intervals is 3
    ASSERT_EQ(track->NIntervals(), 3) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Remove part of the second clip
    m_au3Interaction->removeTracksData({ track->GetId() }, TRACK6_CLIP2_START, (TRACK6_CLIP2_START + TRACK6_CLIP2_END) / 2, false);

    //! [THEN] The number of intervals is still 3
    ASSERT_EQ(track->NIntervals(), 3) << "The number of intervals after the remove operation is not 2";

    //! NOTE: The splitted clip is added on the last index

    //! [THEN] The first and third clips keep the same properties
    const auto firstClip = track->GetClip(0);
    ValidateClipProperties(firstClip, TRACK6_CLIP1_START, TRACK6_CLIP1_END, TRACK6_CLIP1_START, TRACK6_CLIP1_END);
    const auto thirdClip = track->GetClip(1);
    ValidateClipProperties(thirdClip, TRACK6_CLIP3_START, TRACK6_CLIP3_END, TRACK6_CLIP3_START, TRACK6_CLIP3_END);

    //! [THEN] The second clip is splitted and moved forward
    const auto secondClip = track->GetClip(2);
    ValidateClipProperties(secondClip, TRACK6_CLIP2_START, (TRACK6_CLIP2_START + TRACK6_CLIP2_END) / 2, TRACK6_CLIP2_START,
                           (TRACK6_CLIP2_START + TRACK6_CLIP2_END) / 2);

    // Cleanup
    removeTrack(trackThreeClipsId);
}

TEST_F(Au3InteractionTests, RemoveTracksDataRipplePerTrack)
{
    //! [GIVEN] There is a project with a track and a clip
    const auto trackThreeClipsId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackThreeClipsId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackThreeClipsId));

    //! [THEN] The number of intervals is 3
    ASSERT_EQ(track->NIntervals(), 3) << "Precondition failed: The number of intervals is not 2";

    //! [WHEN] Remove part of the second clip
    m_au3Interaction->removeTracksData({ track->GetId() }, TRACK6_CLIP2_START, (TRACK6_CLIP2_START + TRACK6_CLIP2_END) / 2, true);

    //! [THEN] The number of intervals is still 3
    ASSERT_EQ(track->NIntervals(), 3) << "The number of intervals after the remove operation is not 3";

    //! [THEN] The first clip keeps the same properties
    const auto firstClip = track->GetClip(0);
    ValidateClipProperties(firstClip, TRACK6_CLIP1_START, TRACK6_CLIP1_END, TRACK6_CLIP1_START, TRACK6_CLIP1_END);

    //! NOTE: The splitted clip is added on the last index

    //! [THEN] The second clip is splitted and moved forward
    const auto secondClip = track->GetClip(2);
    ValidateClipProperties(secondClip, TRACK6_CLIP2_START, (TRACK6_CLIP2_START + TRACK6_CLIP2_END) / 2, TRACK6_CLIP2_START,
                           (TRACK6_CLIP2_START + TRACK6_CLIP2_END) / 2);

    //! [THEN] The third clip is moved forward as well
    const auto thirdClip = track->GetClip(1);
    ValidateClipProperties(thirdClip, (TRACK6_CLIP2_START + TRACK6_CLIP2_END) / 2,
                           (TRACK6_CLIP2_START + TRACK6_CLIP2_END) / 2 + TRACK6_CLIP3_DURATION,
                           (TRACK6_CLIP2_START + TRACK6_CLIP2_END) / 2,
                           (TRACK6_CLIP2_START + TRACK6_CLIP2_END) / 2 + TRACK6_CLIP3_DURATION);

    // Cleanup
    removeTrack(trackThreeClipsId);
}

TEST_F(Au3InteractionTests, DeleteTracks)
{
    const auto trackThreeClipsId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    const auto trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);

    const auto& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 2) << "Precondition failed: The number of tracks is not 2";

    //! [WHEN] Delete one track
    m_au3Interaction->deleteTracks({ trackThreeClipsId });

    //! [THEN] The number of tracks is decremented
    ASSERT_EQ(projectTracks.Size(), 1) << "The number of tracks after the delete operation is not 4";

    //Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, CopyClipIntoClipboard)
{
    const auto trackId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto clip = track->GetClip(0);

    //! [EXPECT] The clipboard is notified about the new track data
    const ClipKey clipKey { track->GetId(), clip->GetId() };
    EXPECT_CALL(*m_clipboard, addTrackData(Truly([&](const TrackData& data) {
        return data.clipKey == clipKey;
    }))).Times(1);

    //! [WHEN] Copy the tracks into the clipboard
    m_au3Interaction->copyClipIntoClipboard(clipKey);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, CopyContinuousTrackDataIntoClipboard)
{
    const auto trackId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));

    //! [EXPECT] The clipboard is notified about the new data even if outside the clip bounds
    EXPECT_CALL(*m_clipboard, addTrackData(_)).Times(2);

    //! [WHEN] Copy the tracks into the clipboard inside the clip bounds
    m_au3Interaction->copyContinuousTrackDataIntoClipboard(track->GetId(), track->GetClip(0)->GetSequenceStartTime(),
                                                           track->GetClip(0)->GetSequenceEndTime());

    //! [WHEN] Copy the tracks into the clipboard outside the clip bounds
    m_au3Interaction->copyContinuousTrackDataIntoClipboard(track->GetId(), track->GetClip(0)->GetSequenceEndTime() + 1.0,
                                                           track->GetClip(0)->GetSequenceEndTime() + 2.0);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, CopyContinuousTrackDataThrowsWhenStartIsGreaterThanEnd)
{
    const auto trackId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));

    //! [EXPECT] The clipboard is not notified about the new data
    EXPECT_CALL(*m_clipboard, addTrackData(_)).Times(0);

    //! [WHEN] Copy the tracks into the clipboard thrown InconsistencyExpection
    ASSERT_THROW(m_au3Interaction->copyContinuousTrackDataIntoClipboard(track->GetId(), track->GetClip(0)->GetSequenceEndTime(),
                                                                        track->GetClip(0)->GetSequenceStartTime()), InconsistencyException);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, CopyNonContinuousTrackDataIntoClipboard)
{
    const auto trackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));

    //! [EXPECT] The clipboard is notified about the new data
    EXPECT_CALL(*m_clipboard, addTrackData(_)).Times(1);
    EXPECT_CALL(*m_clipboard, setMultiSelectionCopy(true)).Times(1);

    //! [WHEN] Copy the tracks into the clipboard
    std::vector<ClipKey> clips;
    for (size_t i = 0; i < track->NIntervals(); i++) {
        clips.push_back({ track->GetId(), track->GetClip(i)->GetId() });
    }
    m_au3Interaction->copyNonContinuousTrackDataIntoClipboard(track->GetId(), clips, 0);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, CutClipIntoClipboard)
{
    const auto trackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto clip = track->GetClip(1);
    const auto middleClipStart = clip->GetSequenceStartTime();

    //! [GIVEN] Thee number of intervas is 3
    ASSERT_EQ(track->NIntervals(), 3) << "Precondition failed: The number of intervals is not 3";

    //! [EXPECT] The clipboard is notified about the new track data
    const ClipKey clipKey { track->GetId(), clip->GetId() };
    EXPECT_CALL(*m_clipboard, addTrackData(Truly([&](const TrackData& data) {
        return data.clipKey == clipKey;
    }))).Times(1);

    //! [EXPECT] The project is notified about clip removed and track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipRemoved(_)).Times(1);
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(Truly([&](const Track& modifiedTrack) {
        return modifiedTrack.id == trackId;
    }))).Times(1);

    //! [WHEN] Cut the tracks into the clipboard
    m_au3Interaction->cutClipIntoClipboard(clipKey);

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "The number of intervals after the cut operation is not 2";

    //! [THEN] The last clip was moved
    const auto lastClip = track->GetClip(1);
    ValidateClipProperties(lastClip, middleClipStart, middleClipStart + TRACK6_CLIP3_DURATION, middleClipStart,
                           middleClipStart + TRACK6_CLIP3_DURATION);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, CutClipDataIntoClipboardWithoutMovingClips)
{
    const auto trackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto clip = track->GetClip(1);
    const auto middleClipStart = clip->GetSequenceStartTime();
    const auto midleClipEnd = clip->GetSequenceEndTime();

    const auto lastClip = track->GetClip(2);
    const auto lastClipStart = lastClip->GetSequenceStartTime();
    const auto lastClipEnd = lastClip->GetSequenceEndTime();

    //! [GIVEN] Thee number of intervas is 3
    ASSERT_EQ(track->NIntervals(), 3) << "Precondition failed: The number of intervals is not 3";

    //! [EXPECT] The clipboard is notified about the new track data
    EXPECT_CALL(*m_clipboard, addTrackData(_)).Times(1);

    //! [EXPECT] The project is notified about clip removed and track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(Truly([&](const Track& modifiedTrack) {
        return modifiedTrack.id == trackId;
    }))).Times(1);

    //! [WHEN] Cut the tracks into the clipboard
    m_au3Interaction->cutClipDataIntoClipboard({ trackId }, middleClipStart, midleClipEnd, false);

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "The number of intervals after the cut operation is not 2";

    //! [THEN] The last clip was not moved
    const auto modifiedLastClip = track->GetClip(1);
    ValidateClipProperties(modifiedLastClip, lastClipStart, lastClipEnd, lastClipStart, lastClipEnd);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, CutClipDataIntoClipboardMovingClips)
{
    const auto trackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto clip = track->GetClip(1);
    const auto middleClipStart = clip->GetSequenceStartTime();
    const auto midleClipEnd = clip->GetSequenceEndTime();

    const auto lastClip = track->GetClip(2);
    const auto lastClipDuration = lastClip->GetSequenceEndTime() - lastClip->GetSequenceStartTime();

    //! [GIVEN] Thee number of intervas is 3
    ASSERT_EQ(track->NIntervals(), 3) << "Precondition failed: The number of intervals is not 3";

    //! [EXPECT] The clipboard is notified about the new track data
    EXPECT_CALL(*m_clipboard, addTrackData(_)).Times(1);

    //! [EXPECT] The project is notified about clip removed and track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(Truly([&](const Track& modifiedTrack) {
        return modifiedTrack.id == trackId;
    }))).Times(1);

    //! [WHEN] Cut the tracks into the clipboard
    m_au3Interaction->cutClipDataIntoClipboard({ trackId }, middleClipStart, midleClipEnd, true);

    //! [THEN] The number of intervals is 2
    ASSERT_EQ(track->NIntervals(), 2) << "The number of intervals after the cut operation is not 2";

    //! [THEN] The last clip was moved
    const auto modifiedLastClip = track->GetClip(1);
    ValidateClipProperties(modifiedLastClip, middleClipStart, middleClipStart + lastClipDuration, middleClipStart,
                           middleClipStart + lastClipDuration);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, DuplicateClipsOnEmptyList)
{
    EXPECT_EQ(m_au3Interaction->duplicateClips({}), false);
}

TEST_F(Au3InteractionTests, DuplicateSingleClip)
{
    const auto trackId = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto clip = track->GetClip(0);

    //! [EXPECT] The number of tracks is 1
    const auto& projectTracks = Au3TrackList::Get(projectRef());
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
    const auto newTrackId = (*projectTracks.begin())->GetId();
    removeTrack(newTrackId);
}

TEST_F(Au3InteractionTests, DuplicateTracksOnEmptyList)
{
    EXPECT_EQ(m_au3Interaction->duplicateTracks({}), true);
}

TEST_F(Au3InteractionTests, DuplicateTracks)
{
    const auto trackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);

    //! [EXPECT] The number of tracks is 1
    const auto& projectTracks = Au3TrackList::Get(projectRef());
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
    const auto newTrackId = (*projectTracks.begin())->GetId();
    removeTrack(newTrackId);
}

TEST_F(Au3InteractionTests, DuplicateRangeSelectionOnNewTrack)
{
    const auto trackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));

    //! [EXPECT] The number of tracks is 1
    const auto& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 1) << "Precondition failed: The number of tracks is not 1";

    //! [EXPECT] Notify about track added
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackAdded(_)).Times(1);

    //! [WHEN] Duplicate the range selection
    m_au3Interaction->duplicateSelectedOnTracks({ track->GetId() }, TRACK6_CLIP2_START, TRACK6_CLIP2_END);

    //! [THEN] The number of tracks is 2
    ASSERT_EQ(projectTracks.Size(), 2) << "The number of tracks after the duplicate operation is not 2";

    //! [THEN] The new track has once clip
    const auto newTrackId = (*projectTracks.rbegin())->GetId();
    Au3WaveTrack* newTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(newTrackId));
    ASSERT_EQ(newTrack->NIntervals(), 1) << "The number of intervals in the new track is not 1";

    auto newClip = newTrack->GetClip(0);
    ValidateClipProperties(newClip, TRACK6_CLIP2_START, TRACK6_CLIP2_END, TRACK6_CLIP2_START, TRACK6_CLIP2_END);

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
    const auto trackId1 = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId1, INVALID_TRACK) << "Failed to create track";

    const auto trackId2 = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
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
    const auto trackId1 = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId1, INVALID_TRACK) << "Failed to create track";

    const auto trackId2 = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
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
    const auto trackId1 = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId1, INVALID_TRACK) << "Failed to create track";

    const auto trackId2 = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
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
    const auto trackId1 = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId1, INVALID_TRACK) << "Failed to create track";

    const auto trackId2 = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
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
    const auto trackId1 = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId1, INVALID_TRACK) << "Failed to create track";

    const auto trackId2 = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId2, INVALID_TRACK) << "Failed to create track 2";

    const auto trackId3 = createTrack(TestTrackID::TRACK_THREE_CLIPS);
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
    const auto trackId1 = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId1, INVALID_TRACK) << "Failed to create track";

    const auto trackId2 = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId2, INVALID_TRACK) << "Failed to create track 2";

    const auto trackId3 = createTrack(TestTrackID::TRACK_THREE_CLIPS);
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
    const auto trackId1 = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId1, INVALID_TRACK) << "Failed to create track";

    const auto trackId2 = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
    ASSERT_NE(trackId2, INVALID_TRACK) << "Failed to create track 2";

    const auto trackId3 = createTrack(TestTrackID::TRACK_THREE_CLIPS);
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
    const auto trackId1 = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId1, INVALID_TRACK) << "Failed to create track";

    const auto trackId2 = createTrack(TestTrackID::TRACK_SMALL_SILENCE);
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
    const auto trackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    const auto secondsToMove = 500 * SAMPLE_INTERVAL;

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto firstClip = track->GetClip(0);
    const auto firstClipStart = firstClip->GetSequenceStartTime();
    const auto firstClipEnd = firstClip->GetSequenceEndTime();

    const auto middleClip = track->GetClip(1);
    const auto middleClipStart = middleClip->GetSequenceStartTime();
    const auto middleClipEnd = middleClip->GetSequenceEndTime();

    const auto lastClip = track->GetClip(2);
    const auto lastClipStart = lastClip->GetSequenceStartTime();
    const auto lastClipEnd = lastClip->GetSequenceEndTime();

    EXPECT_CALL(*m_selectionController,
                selectedClipsInTrackOrder()).Times(1).WillOnce(Return(ClipKeyList {
            ClipKey { trackId, firstClip->GetId() },
            ClipKey { trackId, middleClip->GetId() },
            ClipKey { trackId, lastClip->GetId() }
        }));

    //! [WHEN] Move the clips right
    m_au3Interaction->moveClips(secondsToMove, 0, true);

    //! [THEN] All clips are moved
    const auto modifiedFirstClip = track->GetClip(0);
    ValidateClipProperties(modifiedFirstClip, firstClipStart + secondsToMove, firstClipEnd + secondsToMove, firstClipStart + secondsToMove,
                           firstClipEnd + secondsToMove);
    const auto modifiedMiddleClip = track->GetClip(1);
    ValidateClipProperties(modifiedMiddleClip, middleClipStart + secondsToMove, middleClipEnd + secondsToMove,
                           middleClipStart + secondsToMove, middleClipEnd + secondsToMove);
    const auto modifiedLastClip = track->GetClip(2);
    ValidateClipProperties(modifiedLastClip, lastClipStart + secondsToMove, lastClipEnd + secondsToMove, lastClipStart + secondsToMove,
                           lastClipEnd + secondsToMove);

    //Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, MoveClipLeftWhenClipIsAtZero)
{
    const auto trackId = createTrack(TestTrackID::TRACK_THREE_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto firstClip = track->GetClip(0);
    const auto firstClipStart = firstClip->GetSequenceStartTime();
    const auto firstClipEnd = firstClip->GetSequenceEndTime();

    const auto middleClip = track->GetClip(1);
    const auto middleClipStart = middleClip->GetSequenceStartTime();
    const auto middleClipEnd = middleClip->GetSequenceEndTime();

    const auto lastClip = track->GetClip(2);
    const auto lastClipStart = lastClip->GetSequenceStartTime();
    const auto lastClipEnd = lastClip->GetSequenceEndTime();

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
    m_au3Interaction->moveClips(-1.0, 0, true);

    //! [THEN] No clip is moved
    const auto modifiedFirstClip = track->GetClip(0);
    ValidateClipProperties(modifiedFirstClip, firstClipStart, firstClipEnd, firstClipStart, firstClipEnd);
    const auto modifiedMiddleClip = track->GetClip(1);
    ValidateClipProperties(modifiedMiddleClip, middleClipStart, middleClipEnd, middleClipStart, middleClipEnd);
    const auto modifiedLastClip = track->GetClip(2);
    ValidateClipProperties(modifiedLastClip, lastClipStart, lastClipEnd, lastClipStart, lastClipEnd);

    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, SplitDeleteOnRangeSelection)
{
    //! [GIVEN] There is a project with a track and a two clips
    const auto trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";

    const auto begin = TRACK3_CLIP1_START + 2 * SAMPLE_INTERVAL;
    const auto end = TRACK3_CLIP1_START + 4 * SAMPLE_INTERVAL;

    //! [WHEN] Split and delete part of the first clip
    m_au3Interaction->splitDeleteSelectedOnTracks({ trackTwoClipsId }, begin, end);

    //! [THEN] The number of intervals should be 3
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));
    ASSERT_EQ(track->NIntervals(), 3) << "The number of intervals after the split delete operation is not 3";

    const auto firstSplittedClip = track->GetClip(1);
    const auto secondSplittedClip = track->GetClip(2);
    ValidateClipProperties(firstSplittedClip, TRACK3_CLIP1_START, TRACK3_CLIP1_END, TRACK3_CLIP1_START, begin);
    ValidateClipProperties(secondSplittedClip, TRACK3_CLIP1_START, TRACK3_CLIP1_END, end, TRACK3_CLIP1_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, SplitDeteleByClipId)
{
    //! [GIVEN] There is a project with a track and two clips
    const auto trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));
    const auto firstClip =  track->GetClip(0);

    //! [WHEN] Split and delete the whole clip
    m_au3Interaction->clipSplitDelete({ trackTwoClipsId, firstClip->GetId() });

    //! [THEN] Clip was removed there is ony one left
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split delete operation is not 1";

    //! [THEN] The remaining clip stays unchanged
    const auto remainingClip = track->GetClip(0);
    ValidateClipProperties(remainingClip, TRACK3_CLIP2_START, TRACK3_CLIP2_END, TRACK3_CLIP2_START, TRACK3_CLIP2_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, SplitCutByClipId)
{
    //! [GIVEN] There is a project with a track and two clips
    const auto trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(1);

    //! [EXPECT] Clip is saved to the clipboard
    EXPECT_CALL(*m_clipboard, addTrackData(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));
    const auto firstClip =  track->GetClip(0);

    //! [WHEN] Split and cut the whole clip
    m_au3Interaction->clipSplitCut({ trackTwoClipsId, firstClip->GetId() });

    //! [THEN] Clip was removed there is ony one left
    ASSERT_EQ(track->NIntervals(), 1) << "The number of intervals after the split delete operation is not 1";

    //! [THEN] The remaining clip stays unchanged
    const auto remainingClip = track->GetClip(0);
    ValidateClipProperties(remainingClip, TRACK3_CLIP2_START, TRACK3_CLIP2_END, TRACK3_CLIP2_START, TRACK3_CLIP2_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, SpliCutOnRangeSelection)
{
    //! [GIVEN] There is a project with a track and two clips
    const auto trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(1);

    //! [EXPECT] Track data is saved to the clipboard
    EXPECT_CALL(*m_clipboard, addTrackData(_)).Times(1);

    const auto begin = TRACK3_CLIP1_START + 2 * SAMPLE_INTERVAL;
    const auto end = TRACK3_CLIP1_START + 4 * SAMPLE_INTERVAL;

    //! [WHEN] Split and cut part of the first clip
    m_au3Interaction->splitCutSelectedOnTracks({ trackTwoClipsId }, begin, end);

    //! [THEN] The number of intervals should be 3
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));
    ASSERT_EQ(track->NIntervals(), 3) << "The number of intervals after the split delete operation is not 3";

    //! [THEN] The clip are splitted and the other remains untouched
    const auto untouchedClip = track->GetIntervalAtTime(TRACK3_CLIP2_START);
    ASSERT_NE(untouchedClip, nullptr) << "The untouched clip is not found";
    ValidateClipProperties(untouchedClip, TRACK3_CLIP2_START, TRACK3_CLIP2_END, TRACK3_CLIP2_START, TRACK3_CLIP2_END);
    const auto firstSplittedClip = track->GetIntervalAtTime(TRACK3_CLIP1_START);
    ASSERT_NE(firstSplittedClip, nullptr) << "The first splitted clip is not found";
    const auto secondSplittedClip = track->GetIntervalAtTime(TRACK3_CLIP1_START + 5 * SAMPLE_INTERVAL);
    ASSERT_NE(secondSplittedClip, nullptr) << "The second splitted clip is not found";
    ASSERT_NE(firstSplittedClip, secondSplittedClip) << "The first and second splitted clips are the same";
    ValidateClipProperties(firstSplittedClip, TRACK3_CLIP1_START, TRACK3_CLIP1_END, TRACK3_CLIP1_START, begin);
    ValidateClipProperties(secondSplittedClip, TRACK3_CLIP1_START, TRACK3_CLIP1_END, end, TRACK3_CLIP1_END);

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
    m_au3Interaction->splitTracksAt({}, 0.0);
}

TEST_F(Au3InteractionTests, SplitTracksOnClipData)
{
    //! [GIVEN] There is a project with a track and two clips
    const auto trackTwoClipsId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackTwoClipsId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackTwoClipsId));

    //! [WHEN] Split the track in the middle of the first clip
    const secs_t pivot = TRACK3_CLIP1_START + 2 * SAMPLE_INTERVAL;
    m_au3Interaction->splitTracksAt({ trackTwoClipsId }, pivot);

    //! [THEN] The number of intervals should be 3
    ASSERT_EQ(track->NIntervals(), 3) << "The number of intervals after the split operation is not 3";

    //! [THEN] The clip are splitted and the other remains untouched
    const auto untouchedClip = track->GetIntervalAtTime(TRACK3_CLIP2_START);
    const auto firstSplittedClip = track->GetIntervalAtTime(pivot - SAMPLE_INTERVAL);
    const auto secondSplittedClip = track->GetIntervalAtTime(pivot + SAMPLE_INTERVAL);

    ASSERT_NE(untouchedClip, nullptr) << "The untouched clip is not found";
    ASSERT_NE(firstSplittedClip, nullptr) << "The first splitted clip is not found";
    ASSERT_NE(secondSplittedClip, nullptr) << "The second splitted clip is not found";
    ASSERT_NE(firstSplittedClip, secondSplittedClip) << "The first and second splitted clips are the same";

    ValidateClipProperties(untouchedClip, TRACK3_CLIP2_START, TRACK3_CLIP2_END, TRACK3_CLIP2_START, TRACK3_CLIP2_END);
    ValidateClipProperties(firstSplittedClip, TRACK3_CLIP1_START, TRACK3_CLIP1_END, TRACK3_CLIP1_START, pivot);
    ValidateClipProperties(secondSplittedClip, TRACK3_CLIP1_START, TRACK3_CLIP1_END, pivot, TRACK3_CLIP1_END);

    // Cleanup
    removeTrack(trackTwoClipsId);
}

TEST_F(Au3InteractionTests, TrimSingleClipLeft)
{
    //! [GIVEN] There is a project with a track and a single clip
    const auto trackId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto firstClip =  track->GetClip(0);

    //! [WHEN] Trim the clip from the left
    const secs_t deltaSec = 2 * SAMPLE_INTERVAL;
    const secs_t minClipDuration = 2 * SAMPLE_INTERVAL;
    m_au3Interaction->trimClipLeft({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clip is trimmed
    const auto trimmedClip = track->GetClip(0);
    ValidateClipProperties(trimmedClip, TRACK5_CLIP_START, TRACK5_CLIP_END, TRACK5_CLIP_START + deltaSec, TRACK5_CLIP_END);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, TrimSingleClipRight)
{
    //! [GIVEN] There is a project with a track and a single clip
    const auto trackId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto firstClip =  track->GetClip(0);

    //! [WHEN] Trim the clip from the right
    const secs_t deltaSec = 2 * SAMPLE_INTERVAL;
    const secs_t minClipDuration = 2 * SAMPLE_INTERVAL;
    m_au3Interaction->trimClipRight({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clip is trimmed
    const auto trimmedClip = track->GetClip(0);
    ValidateClipProperties(trimmedClip, TRACK5_CLIP_START, TRACK5_CLIP_END, TRACK5_CLIP_START, TRACK5_CLIP_END - deltaSec);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, TrimTwoClipsLeftShouldConsiderMinClipDuration)
{
    //! [GIVEN] There is a project with a track and two clips
    const auto trackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(2);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto firstClip =  track->GetClip(0);
    const auto secondClip =  track->GetClip(1);

    //! [GIVEN] Both clips are selected
    ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(ClipKeyList {
            ClipKey { trackId, firstClip->GetId() },
            ClipKey { trackId, secondClip->GetId() }
        }));

    //! [WHEN] Trim the clips from the left
    const secs_t deltaSec = TRACK3_CLIP1_DURATION;
    const secs_t minClipDuration = 2 * SAMPLE_INTERVAL;
    m_au3Interaction->trimClipLeft({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clips are trimmed
    const auto trimmedFirstClip = track->GetClip(0);
    ValidateClipProperties(trimmedFirstClip, TRACK3_CLIP1_START, TRACK3_CLIP1_END, TRACK3_CLIP1_END - minClipDuration, TRACK3_CLIP1_END);

    const auto trimmedSecondClip = track->GetClip(1);
    ValidateClipProperties(trimmedSecondClip, TRACK3_CLIP2_START, TRACK3_CLIP2_END, TRACK3_CLIP2_END - minClipDuration, TRACK3_CLIP2_END);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, TrimTwoClipsRightShouldConsiderMinClipDuration)
{
    //! [GIVEN] There is a project with a track and two clips
    const auto trackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(2);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto firstClip =  track->GetClip(0);
    const auto secondClip =  track->GetClip(1);

    //! [GIVEN] Both clips are selected
    ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(ClipKeyList {
            ClipKey { trackId, firstClip->GetId() },
            ClipKey { trackId, secondClip->GetId() }
        }));

    //! [WHEN] Trim the clips from the right
    const secs_t deltaSec = TRACK3_CLIP1_DURATION;
    const secs_t minClipDuration = 2 * SAMPLE_INTERVAL;
    m_au3Interaction->trimClipRight({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clips are trimmed
    const auto trimmedFirstClip = track->GetClip(0);
    ValidateClipProperties(trimmedFirstClip, TRACK3_CLIP1_START, TRACK3_CLIP1_END, TRACK3_CLIP1_START,
                           TRACK3_CLIP1_START + minClipDuration);

    const auto trimmedSecondClip = track->GetClip(1);
    ValidateClipProperties(trimmedSecondClip, TRACK3_CLIP2_START, TRACK3_CLIP2_END, TRACK3_CLIP2_START,
                           TRACK3_CLIP2_START + minClipDuration);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, StretchSingleClipLeft)
{
    //! [GIVEN] There is a project with a track and a single clip
    const auto trackId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto firstClip =  track->GetClip(0);

    //! [WHEN] Stretch the clip from the left
    //! NOTE: In order to avoid rounding problems while comparing clip properties we stretch the clip by half
    const secs_t deltaSec = TRACK5_CLIP_DURATION / 2;
    const secs_t minClipDuration = 2 * SAMPLE_INTERVAL;
    m_au3Interaction->stretchClipLeft({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clip is stretched
    const auto stretchedClip = track->GetClip(0);
    ValidateClipProperties(stretchedClip, TRACK5_CLIP_START + deltaSec, TRACK5_CLIP_END, TRACK5_CLIP_START + deltaSec, TRACK5_CLIP_END);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, StretchSingleClipRight)
{
    //! [GIVEN] There is a project with a track and a single clip
    const auto trackId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto firstClip =  track->GetClip(0);

    //! [WHEN] Stretch the clip from the right
    //! NOTE: In order to avoid rounding problems while comparing clip properties we stretch the clip by half
    const secs_t deltaSec = TRACK5_CLIP_DURATION / 2;
    const secs_t minClipDuration = 2 * SAMPLE_INTERVAL;
    m_au3Interaction->stretchClipRight({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clip is stretched
    const auto stretchedClip = track->GetClip(0);
    ValidateClipProperties(stretchedClip, TRACK5_CLIP_START, TRACK5_CLIP_END - deltaSec, TRACK5_CLIP_START, TRACK5_CLIP_END - deltaSec);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, StretchTwoClipsLeftShouldConsiderMinClipDuration)
{
    //! [GIVEN] There is a project with a track and two clips
    const auto trackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(2);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto firstClip =  track->GetClip(0);
    const auto secondClip =  track->GetClip(1);

    //! [GIVEN] Both clips are selected
    ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(ClipKeyList {
            ClipKey { trackId, firstClip->GetId() },
            ClipKey { trackId, secondClip->GetId() }
        }));

    //! [WHEN] Stretch the clips from the left
    //! NOTE: In order to avoid rounding problems while comparing clip properties we stretch the clip by half
    const secs_t deltaSec = TRACK3_CLIP1_DURATION;
    const secs_t minClipDuration = TRACK3_CLIP1_DURATION / 2;

    m_au3Interaction->stretchClipLeft({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clips are stretched considering the minimum clip duration
    const auto stretchedFirstClip = track->GetClip(0);
    ValidateClipProperties(stretchedFirstClip, TRACK3_CLIP1_START + minClipDuration, TRACK3_CLIP1_END, TRACK3_CLIP1_START + minClipDuration,
                           TRACK3_CLIP1_END);

    const auto stretchedSecondClip = track->GetClip(1);
    ValidateClipProperties(stretchedSecondClip, TRACK3_CLIP2_START + minClipDuration, TRACK3_CLIP2_END,
                           TRACK3_CLIP2_START + minClipDuration, TRACK3_CLIP2_END);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, StretchTwoClipsRightShouldConsiderMinClipDuration)
{
    //! [GIVEN] There is a project with a track and two clips
    const auto trackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed twice
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(2);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto firstClip =  track->GetClip(0);
    const auto secondClip =  track->GetClip(1);

    //! [GIVEN] Both clips are selected
    ON_CALL(*m_selectionController, selectedClips()).WillByDefault(Return(ClipKeyList {
            ClipKey { trackId, firstClip->GetId() },
            ClipKey { trackId, secondClip->GetId() }
        }));

    //! [WHEN] Stretch the clips from the right
    //! NOTE: In order to avoid rounding problems while comparing clip properties we stretch the clip by half
    const secs_t deltaSec = TRACK3_CLIP1_DURATION;
    const secs_t minClipDuration = TRACK3_CLIP1_DURATION / 2;

    m_au3Interaction->stretchClipRight({ trackId, firstClip->GetId() }, deltaSec, minClipDuration, true);

    //! [THEN] The clips are stretched considering the minimum clip duration
    const auto stretchedFirstClip = track->GetClip(0);
    ValidateClipProperties(stretchedFirstClip, TRACK3_CLIP1_START, TRACK3_CLIP1_END - minClipDuration, TRACK3_CLIP1_START,
                           TRACK3_CLIP1_END - minClipDuration);

    const auto stretchedSecondClip = track->GetClip(1);
    ValidateClipProperties(stretchedSecondClip, TRACK3_CLIP2_START, TRACK3_CLIP2_END - minClipDuration, TRACK3_CLIP2_START,
                           TRACK3_CLIP2_END - minClipDuration);

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
    const auto& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 1) << "The number of tracks after the insert silence operation is not 1";

    const auto newTrackId = (*projectTracks.begin())->GetId();
    Au3WaveTrack* newTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(newTrackId));
    ASSERT_EQ(newTrack->NIntervals(), 1) << "The number of intervals after the insert silence operation is not 1";

    const auto newClip = newTrack->GetIntervalAtTime(begin);
    ASSERT_NE(newClip, nullptr) << "The new clip is not found";
    ValidateClipProperties(newClip, begin, end, begin, end);

    // Cleanup
    removeTrack(newTrackId);
}

TEST_F(Au3InteractionTests, InsertSilenceOnEmptySpace)
{
    //! [GIVEN] There is a project with a track and two clips
    const auto trackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about track changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutTrackChanged(_)).Times(1);

    // [WHEN] Insert silence on the empty space
    const secs_t begin = TRACK3_CLIP2_END + 2 * SAMPLE_INTERVAL;
    const secs_t end = TRACK3_CLIP2_END + 4 * SAMPLE_INTERVAL;
    const secs_t duration = end - begin;
    m_au3Interaction->insertSilence({ trackId }, begin, end, duration);

    // [THEN] Track now has 3 clips
    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    ASSERT_EQ(track->NIntervals(), 3) << "The number of intervals after the insert silence operation is not 3";

    const auto newClip = track->GetIntervalAtTime(begin);
    ASSERT_NE(newClip, nullptr) << "The new clip is not found";
    ValidateClipProperties(newClip, begin, end, begin, end);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, PasteOnEmptyClipboardReturnsError)
{
    //! [EXPECT] The project is not notified about track changed
    EXPECT_CALL(*m_clipboard, trackDataEmpty()).Times(1).WillOnce(Return(true));

    const auto ret = m_au3Interaction->pasteFromClipboard(0.0, true, true);
    ASSERT_EQ(ret, make_ret(Err::TrackEmpty)) << "The return value is not TrackEmpty";
}

TEST_F(Au3InteractionTests, PasteOnEmptyTrack)
{
    //! [GIVEN] There is a project with one track and two clips
    const auto trackId = createTrack(TestTrackID::TRACK_TWO_CLIPS);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";
    const auto track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto firstClip = track->GetClip(0);

    //! [GIVEN] Clipboard contains data
    const auto trackCopy = track->Copy(firstClip->GetSequenceStartTime(), firstClip->GetSequenceEndTime());
    ASSERT_NE(trackCopy, nullptr) << "Failed to copy clip";
    const auto trackData = TrackData { trackCopy, { trackCopy->GetId(), firstClip->GetId() } };

    //! [EXPECT] The clipboard is asked for track data
    EXPECT_CALL(*m_clipboard, trackDataEmpty()).Times(1).WillOnce(Return(false));
    EXPECT_CALL(*m_clipboard, trackDataCopy()).Times(1).WillOnce(Return(std::vector<TrackData> { trackData }));
    EXPECT_CALL(*m_playbackState, playbackPosition()).Times(1).WillOnce(Return(0.0));

    //! [WHEN] Paste from clipboard
    const auto ret = m_au3Interaction->pasteFromClipboard(0.0, true, true);
    ASSERT_EQ(ret, muse::make_ok()) << "The return value is not Ok";

    //! [THEN] The project has a new track with a single clip
    const auto& projectTracks = Au3TrackList::Get(projectRef());
    ASSERT_EQ(projectTracks.Size(), 2) << "The number of tracks after the paste operation is not 2";

    const auto newTrackId = (*projectTracks.rbegin())->GetId();
    Au3WaveTrack* newTrack = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(newTrackId));
    ASSERT_EQ(newTrack->NIntervals(), 1) << "The number of intervals after the paste operation is not 1";

    // Cleanup
    removeTrack(trackId);
    removeTrack(newTrackId);
}

TEST_F(Au3InteractionTests, AddNewMonoTrack)
{
    //! [GIVEN] There is a project with no tracks
    const auto& projectTracks = Au3TrackList::Get(projectRef());
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
    const auto& projectTracks = Au3TrackList::Get(projectRef());
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
    const auto trackId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto firstClip =  track->GetClip(0);

    //! [WHEN] Increase the clip speed
    //! TODO: We should rename the parameter from changeClipSpeed once a value of
    //! 2.0 makes the clip expand and not shrink
    const double speedFactor = 2.0;
    m_au3Interaction->changeClipSpeed({ trackId, firstClip->GetId() }, speedFactor);

    //! [THEN] The clip change the start and end times
    const auto modifiedClip = track->GetClip(0);
    ValidateClipProperties(modifiedClip, TRACK5_CLIP_START, TRACK5_CLIP_START + TRACK5_CLIP_DURATION * speedFactor, TRACK5_CLIP_START,
                           TRACK5_CLIP_START + TRACK5_CLIP_DURATION * speedFactor);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, DecreaseClipSpeed)
{
    //! [GIVEN] There is a project with a track and a single clip
    const auto trackId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto firstClip =  track->GetClip(0);

    //! [WHEN] Decrease the clip speed
    //! TODO: We should rename the parameter from changeClipSpeed once a value of
    //! 0.5 makes the clip shrink and not expand
    const double speedFactor = 0.5;
    m_au3Interaction->changeClipSpeed({ trackId, firstClip->GetId() }, speedFactor);

    //! [THEN] The clip change the start and end times
    const auto modifiedClip = track->GetClip(0);
    ValidateClipProperties(modifiedClip, TRACK5_CLIP_START, TRACK5_CLIP_START + TRACK5_CLIP_DURATION * speedFactor, TRACK5_CLIP_START,
                           TRACK5_CLIP_START + TRACK5_CLIP_DURATION * speedFactor);

    // Cleanup
    removeTrack(trackId);
}

TEST_F(Au3InteractionTests, ResetClipSpeed)
{
    //! [GIVEN] There is a project with a track and a single clip
    const auto trackId = createTrack(TestTrackID::TRACK_SILENCE_AT_END);
    ASSERT_NE(trackId, INVALID_TRACK) << "Failed to create track";

    Au3WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), Au3TrackId(trackId));
    const auto firstClip =  track->GetClip(0);

    //! [GIVEN] The clip speed is not the default
    const double speedFactor = 2.0;
    m_au3Interaction->changeClipSpeed({ trackId, firstClip->GetId() }, speedFactor);

    //! [EXPECT] The project is notified about clip changed
    EXPECT_CALL(*m_trackEditProject, notifyAboutClipChanged(_)).Times(1);

    //! [WHEN] Reseting the clip speed
    m_au3Interaction->resetClipSpeed({ trackId, firstClip->GetId() });

    //! [THEN] The clip start and end time are the same as the original clip
    const auto modifiedClip = track->GetClip(0);
    ValidateClipProperties(modifiedClip, TRACK5_CLIP_START, TRACK5_CLIP_END, TRACK5_CLIP_START, TRACK5_CLIP_END);

    // Cleanup
    removeTrack(trackId);
}
}
