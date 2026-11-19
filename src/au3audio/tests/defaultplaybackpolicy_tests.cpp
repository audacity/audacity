/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "au3-audio-devices/AudioIOBase.h"
#include "au3-audio-io/PlaybackSchedule.h"
#include "au3-time-frequency-selection/ViewInfo.h"
#include "au3-track/Track.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/au3project.h"
#include "au3wrap/tests/tracktemplatefactory.h"
#include "project/tests/testtools.h"

#include "au3audio/internal/au3audioengine.h"
#include "au3audio/internal/defaultplaybackpolicy.h"

namespace au::au3audio {
constexpr double SAMPLE_RATE = 44100.0;
constexpr double TRACK_DURATION = 0.5;
constexpr double LOOP_START = 0.3;
constexpr double LOOP_END = 0.8;
constexpr double TIME_TOLERANCE = 1e-9;

/**
 * @brief Fixture driving a DefaultPlaybackPolicy the way AudioIO does, without a stream
 * @details The policy comes from Au3AudioEngine::defaultTransportOptions, so the values the
 *          engine feeds it (end times, loop state) are under test too. A real headless
 *          project provides the play region and the track list. The track ends at 0.5 and
 *          the loop region {0.3, 0.8} reaches past it
 */
class DefaultPlaybackPolicyTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_projectAccessor = std::make_shared<au3::Au3ProjectAccessor>(muse::modularity::globalCtx());
        const std::string source = std::string(au3audio_tests_DATA_ROOT) + "/../../trackedit/tests/data/empty.aup4";
        m_workingProjectPath = std::string(au3audio_tests_DATA_ROOT) + "/playbackpolicy_working.aup4";
        testtools::removeProjectIfExists(m_workingProjectPath);
        ASSERT_TRUE(testtools::copyFile(source, m_workingProjectPath));
        constexpr auto discardAutosave = false;
        ASSERT_TRUE(m_projectAccessor->load(muse::io::path_t(m_workingProjectPath), discardAutosave));

        //! [GIVEN] The project has one track with audio ending at TRACK_DURATION
        au3::TrackTemplateFactory factory(projectRef(), SAMPLE_RATE);
        factory.addTrackFromTemplate("track", {
                { 0.0, { { TRACK_DURATION, au3::TrackTemplateFactory::createNoise } } }
            });

        m_schedule = std::make_unique<PlaybackSchedule>();
    }

    void TearDown() override
    {
        //! NOTE: the policy subscribes to the project's play region, so it goes first
        m_schedule.reset();

        au3::Au3TrackList::Get(projectRef()).Clear();
        m_projectAccessor->clearSavedState();
        m_projectAccessor->close();
        m_projectAccessor.reset();
        testtools::removeProjectIfExists(m_workingProjectPath);
    }

    au3::Au3Project& projectRef() const
    {
        return *reinterpret_cast<au3::Au3Project*>(m_projectAccessor->au3ProjectPtr());
    }

    PlayRegion& playRegion() const
    {
        return ViewInfo::Get(projectRef()).playRegion;
    }

    //! NOTE: the loop region is au3's play region remembered as "last active"; `active` is the loop toggle
    void setLoopRegion(double start, double end, bool active)
    {
        playRegion().SetAllTimes(start, end);
        playRegion().SetActive(active);
    }

    //! Start a default-policy stream from `streamStartTime`, as AudioIO::StartStream does
    DefaultPlaybackPolicy& startStream(double streamStartTime)
    {
        AudioIOStartTransportOptions options = Au3AudioEngine::defaultTransportOptions(projectRef(), true);
        options.pStartTime = streamStartTime;

        m_schedule->Init(playRegion().GetStart(), playRegion().GetEnd(), options, nullptr);
        PlaybackPolicy& policy = m_schedule->GetPolicy();
        policy.Initialize(*m_schedule, SAMPLE_RATE);

        m_schedule->SetSequenceTime(streamStartTime);
        policy.OffsetSequenceTime(*m_schedule, 0.0);
        m_schedule->mTimeQueue.Prime(m_schedule->GetSequenceTime());

        return static_cast<DefaultPlaybackPolicy&>(policy);
    }

    //! The producer thread's pass that picks up a play region change
    void reposition(PlaybackPolicy& policy)
    {
        const PlaybackPolicy::Mixers noMixers;
        policy.RepositionPlayback(*m_schedule, noMixers, 0, 0);
    }

    std::shared_ptr<au3::Au3ProjectAccessor> m_projectAccessor;
    std::string m_workingProjectPath;
    std::unique_ptr<PlaybackSchedule> m_schedule;
};

/**
 * @brief SPEC (issue #11477, PR #12755): turning looping off mid-stream continues to the playable end
 * @details The loop region {0.3, 0.8} reaches past the track end (0.5). When the user turns
 *          looping off while the stream runs, playback must go on to the loop region end,
 *          because a disabled loop region still marks the play range. Ending at the track
 *          end here is the bug, not the spec
 */
TEST_F(DefaultPlaybackPolicyTests, LoopTurnedOffWhilePlaying_ContinuesToThePlayableEnd)
{
    //! [GIVEN] An active loop region reaching past the track end
    setLoopRegion(LOOP_START, LOOP_END, true);

    //! [GIVEN] A stream started before the loop region is looping
    DefaultPlaybackPolicy& policy = startStream(0.1);
    ASSERT_NEAR(m_schedule->mT1, LOOP_END, TIME_TOLERANCE);

    //! [WHEN] Looping is turned off (the play region keeps its times) and the producer sees it
    playRegion().SetActive(false);
    reposition(policy);

    //! [THEN] The stream ends at the loop region end, not at the track end
    EXPECT_NEAR(m_schedule->mT1, LOOP_END, TIME_TOLERANCE);
    EXPECT_FALSE(policy.Looping(*m_schedule));
}

/**
 * @brief SPEC (issue #11477, PR #12755): a seek during non-looped playback keeps the playable end
 * @details With an inactive loop region past the track end, the player collapses the play
 *          region onto the seek position. The policy picks that up as a region change and
 *          must keep running to the loop region end, not fall back to the track end
 */
TEST_F(DefaultPlaybackPolicyTests, SeekWithAnInactiveLoopRegion_ContinuesToThePlayableEnd)
{
    //! [GIVEN] An inactive loop region reaching past the track end
    setLoopRegion(LOOP_START, LOOP_END, false);

    //! [GIVEN] The play region runs from the cursor (0.1) to the loop region end, as the player sets it
    playRegion().SetStart(0.1);
    playRegion().SetEnd(LOOP_END);
    DefaultPlaybackPolicy& policy = startStream(0.1);
    ASSERT_NEAR(m_schedule->mT1, LOOP_END, TIME_TOLERANCE);

    //! [WHEN] A seek collapses the inactive play region onto the new position (Au3Player::seek)
    //! and the producer sees it
    playRegion().SetStart(0.2);
    playRegion().SetEnd(0.2);
    reposition(policy);

    //! [THEN] The stream still ends at the loop region end, not at the track end
    EXPECT_NEAR(m_schedule->mT1, LOOP_END, TIME_TOLERANCE);
}

/**
 * @brief SPEC (PR #12755): a seek before the loop region while looping lands where asked
 * @details A ruler click before the loop must not be clamped onto the loop start: playback
 *          continues from the clicked position, runs into the loop region and loops from there,
 *          exactly like a stream started from a cursor before the loop
 */
TEST_F(DefaultPlaybackPolicyTests, SeekBeforeTheLoopWhileLooping_LandsThere)
{
    //! [GIVEN] An active loop region and a stream started before it
    setLoopRegion(LOOP_START, LOOP_END, true);
    DefaultPlaybackPolicy& policy = startStream(0.1);

    //! [GIVEN] Playback has entered the loop region
    m_schedule->SetSequenceTime(0.5);

    //! [WHEN] The user seeks back before the loop region (AudioIO::CallbackDoSeek)
    const double landed = policy.OffsetSequenceTime(*m_schedule, -0.4);

    //! [THEN] The seek lands where asked, not on the loop start
    EXPECT_NEAR(landed, 0.1, TIME_TOLERANCE);

    //! [THEN] The current pass runs from there to the loop end before wrapping
    EXPECT_NEAR(m_schedule->RealTimeRemaining(), LOOP_END - 0.1, TIME_TOLERANCE);
}

/**
 * @brief A seek past the loop region end while looping is clamped to the loop end
 * @details Pins the upper bound that stays: the schedule has nothing past its end
 */
TEST_F(DefaultPlaybackPolicyTests, SeekPastTheLoopEndWhileLooping_IsClampedToTheLoopEnd)
{
    //! [GIVEN] An active loop region and a stream looping inside it
    setLoopRegion(LOOP_START, LOOP_END, true);
    DefaultPlaybackPolicy& policy = startStream(0.1);
    m_schedule->SetSequenceTime(0.5);

    //! [WHEN] The user seeks past the loop region end
    const double landed = policy.OffsetSequenceTime(*m_schedule, 1.0);

    //! [THEN] The seek is clamped to the loop region end
    EXPECT_NEAR(landed, LOOP_END, TIME_TOLERANCE);
}
} // namespace au::au3audio
