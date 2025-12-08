/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include "context/tests/mocks/globalcontextmock.h"
#include "context/tests/mocks/playbackstatemock.h"
#include "project/tests/mocks/audacityprojectmock.h"
#include "mocks/trackeditprojectmock.h"

#include "tracktemplatefactory.h"

#include "au3wrap/internal/au3project.h"
#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/domconverter.h"

using ::testing::NiceMock;
using ::testing::Return;

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

/**
 * @brief Base class for Au3 interaction tests
 * Provides common functionality for testing Au3 interaction classes:
 * - Track template creation and management
 * - Project setup and teardown
 * - Clip property validation
 */
class Au3InteractionTestBase : public ::testing::Test
{
public:
    void initTestProject()
    {
        m_au3ProjectAccessor = std::make_shared<au3::Au3ProjectAccessor>();
        const muse::io::path_t TEST_PROJECT_PATH = muse::String::fromUtf8(trackedit_tests_DATA_ROOT) + "/data/empty.aup3";
        constexpr auto discardAutosave = false;
        muse::Ret ret = m_au3ProjectAccessor->load(TEST_PROJECT_PATH, discardAutosave);

        ON_CALL(*m_currentProject, au3ProjectPtr())
        .WillByDefault(Return(m_au3ProjectAccessor->au3ProjectPtr()));

        ON_CALL(*m_trackEditProject, trackList())
            .WillByDefault(testing::Invoke([this](){
                std::vector<Track> result;

                auto& tracks = Au3TrackList::Get(projectRef());

                for (const Au3Track* t : tracks) {
                    Track au4t = au3::DomConverter::track(t);
                    result.push_back(std::move(au4t));
                }

                return result;
            }));
    }

    TrackId createTrack(const TestTrackID testTrackID)
    {
        TrackTemplateFactory factory(projectRef(), DEFAULT_SAMPLE_RATE);

        TrackId trackId = INVALID_TRACK;
        switch (testTrackID) {
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
        Au3Track* track = DomAccessor::findTrack(projectRef(), Au3TrackId(trackId));
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

protected:
    std::shared_ptr<context::GlobalContextMock> m_globalContext;
    std::shared_ptr<project::AudacityProjectMock> m_currentProject;
    std::shared_ptr<TrackeditProjectMock> m_trackEditProject;
    std::shared_ptr<context::PlaybackStateMock> m_playbackState;

    std::shared_ptr<au3::Au3ProjectAccessor> m_au3ProjectAccessor;
};
}
