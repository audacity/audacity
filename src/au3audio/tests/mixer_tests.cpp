/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "au3-clip-processing/PlayableSequenceFactory.h"
#include "au3-mixer/Mix.h"
#include "au3-track/Track.h"
#include "au3-wave-track/WaveTrack.h"

#include "au3wrap/au3types.h"
#include "au3wrap/internal/au3project.h"
#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/tests/tracktemplatefactory.h"
#include "project/tests/testtools.h"

namespace au::au3audio {
constexpr double SAMPLE_RATE = 44100.0;
constexpr double CLIP_DURATION = 0.5;
constexpr double LOOP_START = 0.3;
constexpr double LOOP_END = 0.8;
constexpr size_t BUFFER_SIZE = 256;
constexpr double TIME_TOLERANCE = 1.0 / SAMPLE_RATE;

/**
 * @brief Fixture mixing a clip whose samples encode their own time
 * @details Every sample holds its own time in seconds, so the first sample of a processed
 *          buffer tells where the mixer fetched from. The mixer is bounded like a looping
 *          playback stream: [LOOP_START, LOOP_END]
 */
class MixerTests : public ::testing::Test
{
protected:
    void SetUp() override
    {
        m_projectAccessor = std::make_shared<au3::Au3ProjectAccessor>(muse::modularity::globalCtx());
        const std::string source = std::string(au3audio_tests_DATA_ROOT) + "/../../trackedit/tests/data/empty.aup4";
        m_workingProjectPath = std::string(au3audio_tests_DATA_ROOT) + "/mixer_working.aup4";
        testtools::removeProjectIfExists(m_workingProjectPath);
        ASSERT_TRUE(testtools::copyFile(source, m_workingProjectPath));
        constexpr auto discardAutosave = false;
        ASSERT_TRUE(m_projectAccessor->load(muse::io::path_t(m_workingProjectPath), discardAutosave));

        //! [GIVEN] One clip [0, CLIP_DURATION] whose samples hold their own time
        const auto timeRamp = [](double duration, double sampleRate) {
            std::vector<float> samples(static_cast<size_t>(duration * sampleRate));
            for (size_t i = 0; i < samples.size(); ++i) {
                samples[i] = static_cast<float>(i / sampleRate);
            }
            return samples;
        };
        au3::TrackTemplateFactory factory(projectRef(), SAMPLE_RATE);
        const au3::Au3TrackId trackId = factory.addTrackFromTemplate("track", {
                { 0.0, { { CLIP_DURATION, timeRamp } } }
            });
        m_track = au3::DomAccessor::findWaveTrack(projectRef(), trackId);
        ASSERT_NE(m_track, nullptr);
    }

    void TearDown() override
    {
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

    //! A playback-like mixer over the clip, bounded to [start, end]
    std::unique_ptr<Mixer> makeMixer(double start, double end) const
    {
        Mixer::Inputs inputs;
        inputs.emplace_back(PlayableSequenceFactory::Create(*m_track, m_track->GetClipInterfaces()), Mixer::Stages {});
        return std::make_unique<Mixer>(std::move(inputs), std::nullopt, true, Mixer::WarpOptions { &projectRef() },
                                       start, end, m_track->NChannels(), BUFFER_SIZE, false, SAMPLE_RATE, floatSample);
    }

    //! The time the next processed buffer starts from
    static double nextBufferTime(Mixer& mixer)
    {
        const size_t produced = mixer.Process(BUFFER_SIZE);
        EXPECT_GT(produced, 0u);
        return reinterpret_cast<const float*>(mixer.GetBuffer(0))[0];
    }

    std::shared_ptr<au3::Au3ProjectAccessor> m_projectAccessor;
    std::string m_workingProjectPath;
    au3::Au3WaveTrack* m_track = nullptr;
};

/**
 * @brief Reposition cannot leave the mixer bounds
 * @details Pins why a seek needs more than Reposition: a looping playback policy narrows
 *          the mixer bounds to the loop region, so repositioning before the loop start kept
 *          fetching from the loop start (the cause of a ruler click before an active loop
 *          landing on the loop start, PR #12755)
 */
TEST_F(MixerTests, Reposition_BeforeTheBounds_ClampsToTheStart)
{
    //! [GIVEN] A mixer bounded to the loop region, fetching from its start
    std::unique_ptr<Mixer> mixer = makeMixer(LOOP_START, LOOP_END);
    ASSERT_NEAR(nextBufferTime(*mixer), LOOP_START, TIME_TOLERANCE);

    //! [WHEN] It is repositioned before the bounds
    mixer->Reposition(0.1, true);

    //! [THEN] It keeps fetching from the lower bound
    EXPECT_NEAR(nextBufferTime(*mixer), LOOP_START, TIME_TOLERANCE);
}

/**
 * @brief SetTimes widens the bounds and fetches from the new start
 * @details SPEC (PR #12755): AudioIO::CallbackDoSeek resets the mixer bounds to
 *          [seek time, schedule end] so an in-stream seek may leave the loop region
 */
TEST_F(MixerTests, SetTimes_BeforeTheBounds_FetchesFromThere)
{
    //! [GIVEN] A mixer bounded to the loop region, fetching from its start
    std::unique_ptr<Mixer> mixer = makeMixer(LOOP_START, LOOP_END);
    ASSERT_NEAR(nextBufferTime(*mixer), LOOP_START, TIME_TOLERANCE);

    //! [WHEN] Its bounds are reassigned to start before the loop region
    mixer->SetTimes(0.1, LOOP_END, true);

    //! [THEN] It fetches from the new start
    EXPECT_NEAR(nextBufferTime(*mixer), 0.1, TIME_TOLERANCE);
}

/**
 * @brief SetTimesAndSpeed still reassigns the bounds and repositions
 */
TEST_F(MixerTests, SetTimesAndSpeed_BeforeTheBounds_FetchesFromThere)
{
    //! [GIVEN] A mixer bounded to the loop region, fetching from its start
    std::unique_ptr<Mixer> mixer = makeMixer(LOOP_START, LOOP_END);
    ASSERT_NEAR(nextBufferTime(*mixer), LOOP_START, TIME_TOLERANCE);

    //! [WHEN] Its bounds and speed are reassigned to start before the loop region
    mixer->SetTimesAndSpeed(0.2, LOOP_END, 1.0, true);

    //! [THEN] It fetches from the new start
    EXPECT_NEAR(nextBufferTime(*mixer), 0.2, TIME_TOLERANCE);
}
} // namespace au::au3audio
