/*
* Audacity: A Digital Audio Editor
*/

#include <gtest/gtest.h>
#include <QFile>
#include <QTemporaryDir>

#include <vector>

#include "au3wrap/internal/au3project.h"
#include "au3-project/Project.h"
#include "au3-stretching-sequence/ClipFades.h"
#include "au3-stretching-sequence/FadingSequence.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"
#include "testing/testcontext.h"

namespace {
constexpr auto TEST_SAMPLE_RATE = 1000;
constexpr auto TEST_CLIP_START = 0.5;
constexpr auto TEST_CLIP_DURATION = 1.0;
constexpr auto TEST_FADE_IN = 0.2;
constexpr auto TEST_FADE_OUT = 0.1;
}

namespace au::project {
class FadingSequenceTests : public ::testing::Test
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
        const auto path = m_directory.filePath("fades.aup4");
        ASSERT_TRUE(QFile::copy(QString::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup4", path));
        ASSERT_TRUE(m_accessor.load(path.toStdString(), false));

        //! A clip of ones, so that what we read is the fade gain
        m_track = WaveTrackFactory::Get(project()).Create(floatSample, TEST_SAMPLE_RATE);
        m_clip = m_track->CreateClip(TEST_CLIP_START, "clip");
        const std::vector<float> ones(TEST_CLIP_DURATION * TEST_SAMPLE_RATE, 1.f);
        auto data = reinterpret_cast<constSamplePtr>(ones.data());
        m_clip->Append(&data, floatSample, ones.size(), 1, floatSample);
        m_clip->Flush();
        m_track->InsertInterval(m_clip, true);

        auto& fades = ClipFades::Get(*m_clip);
        fades.setFadeIn(TEST_FADE_IN);
        fades.setFadeOut(TEST_FADE_OUT);
    }

    void TearDown() override
    {
        m_accessor.close();
    }

    std::vector<float> read(const PlayableSequence& sequence, sampleCount start, size_t len, bool backwards)
    {
        std::vector<float> buffer(len);
        const auto ptr = reinterpret_cast<samplePtr>(buffer.data());
        EXPECT_TRUE(sequence.DoGet(0, 1, &ptr, floatSample, start, len, backwards));
        return buffer;
    }
};

TEST_F(FadingSequenceTests, LinearFadesAreAppliedReadingForward)
{
    const auto sequence = FadingSequence::Create(*m_track, m_track->GetClipInterfaces());
    const auto samples = read(*sequence, 0, 2000, false);

    EXPECT_FLOAT_EQ(samples[499], 0.f); // before the clip
    EXPECT_FLOAT_EQ(samples[500], 0.f); // the fade-in starts from silence
    EXPECT_NEAR(samples[600], 0.5f, 1e-6); // halfway through the fade-in
    EXPECT_FLOAT_EQ(samples[700], 1.f); // after the fade-in
    EXPECT_FLOAT_EQ(samples[1400], 1.f); // before the fade-out
    EXPECT_NEAR(samples[1450], 0.5f, 1e-6); // halfway through the fade-out
    EXPECT_NEAR(samples[1499], 0.01f, 1e-6); // last sample of the clip
}

TEST_F(FadingSequenceTests, LinearFadesAreAppliedReadingBackward)
{
    const auto sequence = FadingSequence::Create(*m_track, m_track->GetClipInterfaces());
    // From sample 1500 (exclusive) down to 500, in reverse order
    const auto samples = read(*sequence, 1500, 1000, true);

    EXPECT_NEAR(samples[0], 0.01f, 1e-6); // sample 1499
    EXPECT_NEAR(samples[50], 0.51f, 1e-6); // sample 1449
    EXPECT_FLOAT_EQ(samples[500], 1.f); // sample 999
    EXPECT_NEAR(samples[899], 0.5f, 1e-6); // sample 600
}

TEST_F(FadingSequenceTests, FadeChangesApplyToTheNextRead)
{
    const auto sequence = FadingSequence::Create(*m_track, m_track->GetClipInterfaces());
    EXPECT_FLOAT_EQ(read(*sequence, 0, 1000, false)[700], 1.f);

    //! [WHEN] the fade-in is lengthened while the sequence is in use, e.g. during playback
    ClipFades::Get(*m_clip).setFadeIn(0.4);

    //! [THEN] the next read has it
    EXPECT_NEAR(read(*sequence, 1000, 1000, false)[0], 1.f, 1e-6); // sample 1000, after the new fade-in
    EXPECT_NEAR(read(*sequence, 0, 1000, false)[700], 0.5f, 1e-6); // sample 700, halfway through it
}
}
