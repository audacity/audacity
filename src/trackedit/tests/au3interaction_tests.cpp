/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "../internal/au3/au3interaction.h"

#include "context/tests/mocks/globalcontextmock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "mocks/trackeditprojectmock.h"
#include "mocks/projecthistorymock.h"
#include "tracktemplatefactory.h"

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
 * Each layout represents a specific use case.
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
 * What should we expect from the tests?
 *
 * - Split operations only work when the silence is large enough. (0.01s)
 * - If the silence is too short, the operation should not be performed.
 * - The splitted clips should have the same duration as the original clip
 * but trimmed in the left or right edge.
 * - If executed in a single clip with silence at the beginning or end, the
 * duration will be maintained but the clip will be trimmed at the start or end.
 * - Merge operations should join two clips into a single clip with the same
 * duration as the sum of the two clips.
 * - Split range selection into new tracks only adds a track if the interval
 * contains clip data.
 * - Split into new track should keep the clip / selection start time.
 * - Remove operations clears the content of a track.
 * This data cannot be recovered later by untrimming a clip.
 * - RemoveTracksData can use ripple per clip or per track strategy selected by
 * a boolean parameter moveClips.
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

        m_au3Interaction->globalContext.set(m_globalContext);
        m_au3Interaction->projectHistory.set(m_projectHistory);

        m_trackEditProject = std::make_shared<NiceMock<TrackeditProjectMock> >();
        ON_CALL(*m_globalContext, currentTrackeditProject())
        .WillByDefault(Return(m_trackEditProject));

        m_currentProject = std::make_shared<NiceMock<project::AudacityProjectMock> >();
        ON_CALL(*m_globalContext, currentProject())
        .WillByDefault(Return(m_currentProject));

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

    std::shared_ptr<Au3Interaction> m_au3Interaction;

    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<project::AudacityProjectMock> m_currentProject;
    std::shared_ptr<TrackeditProjectMock> m_trackEditProject;
    std::shared_ptr<ProjectHistoryMock> m_projectHistory;

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
}
