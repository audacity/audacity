/*
 * Audacity: A Digital Audio Editor
 */

#include <gtest/gtest.h>
#include <QFile>
#include <QTemporaryDir>

#include <vector>

#include "au3wrap/internal/au3project.h"
#include "au3-project/Project.h"
#include "au3-clip-processing/SequenceProcessingGraphFactory.h"
#include "au3-mixer/Envelope.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"
#include "testing/testcontext.h"

namespace {
constexpr auto TEST_SAMPLE_RATE = 1000;
constexpr auto TEST_CLIP_START = 0.5;
constexpr auto TEST_CLIP_DURATION = 1.0;
//! Envelope points, in the time of the track
constexpr auto TEST_FIRST_POINT_TIME = TEST_CLIP_START + 0.2;
constexpr auto TEST_FIRST_POINT_VALUE = 0.25;
constexpr auto TEST_SECOND_POINT_TIME = TEST_CLIP_START + 0.6;
constexpr auto TEST_SECOND_POINT_VALUE = 2.0;
constexpr auto TEST_NUM_CHECKS = 10;

constexpr auto TEST_TOLERANCE = 1e-3;

constexpr auto clipStartSample = static_cast<size_t>(TEST_CLIP_START * TEST_SAMPLE_RATE);
constexpr auto clipEndSample = static_cast<size_t>((TEST_CLIP_START + TEST_CLIP_DURATION) * TEST_SAMPLE_RATE);
}

namespace au::project {
class PlaybackEnvelopeTests : public ::testing::Test
{
protected:
    QTemporaryDir m_directory;
    au3::Au3ProjectAccessor m_accessor { au::testutils::makeTestContext() };
    std::shared_ptr<WaveTrack> m_track;
    std::shared_ptr<WaveClip> m_clip;

    ::AudacityProject& project()
    {
        return *reinterpret_cast<::AudacityProject*>(m_accessor.au3ProjectPtr());
    }

    void SetUp() override
    {
        const auto path = m_directory.filePath("envelope.aup4");
        ASSERT_TRUE(QFile::copy(QString::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup4", path));
        ASSERT_TRUE(m_accessor.load(path.toStdString(), false));

        //! A clip of ones, so that what we read is the envelope's value
        m_track = WaveTrackFactory::Get(project()).Create(floatSample, TEST_SAMPLE_RATE);
        m_clip = m_track->CreateClip(TEST_CLIP_START, "clip");
        const std::vector<float> ones(TEST_CLIP_DURATION * TEST_SAMPLE_RATE, 1.f);
        auto data = reinterpret_cast<constSamplePtr>(ones.data());
        m_clip->Append(&data, floatSample, ones.size(), 1, floatSample);
        m_clip->Flush();
        m_track->InsertInterval(m_clip, true);

        auto& envelope = m_clip->GetEnvelope();
        envelope.InsertOrReplace(TEST_FIRST_POINT_TIME, TEST_FIRST_POINT_VALUE);
        envelope.InsertOrReplace(TEST_SECOND_POINT_TIME, TEST_SECOND_POINT_VALUE);
    }

    void TearDown() override
    {
        m_accessor.close();
    }

    std::vector<float> read(ISequenceProcessingNode& graph, sampleCount start, size_t len)
    {
        std::vector<float> buffer(len);
        float* const buffers[] { buffer.data() };
        graph.GetSamples(buffers, start, len);
        return buffer;
    }

    //! Expects the samples of the clip to be the envelope's values at their times, times `level`
    void expectEnvelope(const std::vector<float>& samples, size_t begin, size_t end, double level)
    {
        const auto& envelope = m_clip->GetEnvelope();
        for (auto i = 0; i < TEST_NUM_CHECKS; ++i) {
            const auto sample = begin + i * (end - begin) / TEST_NUM_CHECKS;
            const auto time = static_cast<double>(sample) / TEST_SAMPLE_RATE;
            EXPECT_NEAR(samples[sample], level * envelope.GetValue(time), TEST_TOLERANCE) << "at " << time;
        }
    }
};

TEST_F(PlaybackEnvelopeTests, EnvelopeIsAppliedAtTheTimesOfTheSamples)
{
    const auto graph = SequenceProcessingGraphFactory::CreateGraph(*m_track, m_track->GetClipInterfaces());
    const auto samples = read(*graph, 0, clipEndSample);

    EXPECT_FLOAT_EQ(samples[clipStartSample - 1], 0.f);
    EXPECT_NEAR(samples[static_cast<size_t>(TEST_FIRST_POINT_TIME * TEST_SAMPLE_RATE)], TEST_FIRST_POINT_VALUE, TEST_TOLERANCE);
    EXPECT_NEAR(samples[static_cast<size_t>(TEST_SECOND_POINT_TIME * TEST_SAMPLE_RATE)], TEST_SECOND_POINT_VALUE, TEST_TOLERANCE);
    constexpr auto unitLevel = 1.0;
    expectEnvelope(samples, clipStartSample, clipEndSample, unitLevel);
}

TEST_F(PlaybackEnvelopeTests, EnvelopesOfStretchedClipsAreInPlayTime)
{
    //! [GIVEN] the clip stretched, and its envelope with it
    constexpr auto stretchRatio = 2.0;
    m_clip->StretchBy(stretchRatio);

    //! [WHEN] it is read
    const auto stretchedEndSample = clipStartSample + static_cast<size_t>(stretchRatio * TEST_CLIP_DURATION * TEST_SAMPLE_RATE);
    const auto graph = SequenceProcessingGraphFactory::CreateGraph(*m_track, m_track->GetClipInterfaces());
    const auto samples = read(*graph, 0, stretchedEndSample);

    //! [THEN] the samples follow the stretched envelope, away from where the stretcher starts and ends. Stretching
    //! changes the level a bit: it is measured past the last point, where the envelope is flat.
    const auto stretchedSample = [&](double time) {
        return clipStartSample + static_cast<size_t>(stretchRatio * (time - TEST_CLIP_START) * TEST_SAMPLE_RATE);
    };
    const auto flatSample = (stretchedSample(TEST_SECOND_POINT_TIME) + stretchedEndSample) / 2;
    const auto level = samples[flatSample] / TEST_SECOND_POINT_VALUE;
    expectEnvelope(samples, stretchedSample(TEST_FIRST_POINT_TIME), flatSample, level);
}
}
