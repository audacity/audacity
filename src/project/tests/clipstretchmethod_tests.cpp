/*
 * Audacity: A Digital Audio Editor
 */

#include <cmath>
#include <vector>

#include <gtest/gtest.h>
#include <QFile>
#include <QTemporaryDir>

#include "au3wrap/internal/au3project.h"
#include "au3-project/Project.h"
#include "au3-project-file-io/ProjectFileIO.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-xml/XMLFileReader.h"
#include "au3-xml/XMLWriter.h"
#include "testing/testcontext.h"

namespace {
constexpr auto TEST_SAMPLE_RATE = 48000;
//! 100 samples per period at the test rate
constexpr auto TEST_FREQUENCY = 480.0;

//! A project with one track holding a one-second clip; `clipAttributes` go into its `waveclip` tag
wxString projectXml(const wxString& clipAttributes)
{
    return wxString::Format(
        "<project version=\"1.3.0\" audacityversion=\"4.0.0\">"
        "<wavetrack rate=\"%d\" sampleformat=\"262159\" channel=\"0\" linked=\"0\">"
        "<waveclip offset=\"0\" trimLeft=\"0\" trimRight=\"0\" clipStretchRatio=\"1\" %s>"
        "<sequence maxsamples=\"262144\" sampleformat=\"262159\" numsamples=\"%d\">"
        "<waveblock start=\"0\" blockid=\"-%d\"/>"
        "</sequence><envelope numpoints=\"0\"/></waveclip>"
        "</wavetrack></project>",
        TEST_SAMPLE_RATE, clipAttributes, TEST_SAMPLE_RATE, TEST_SAMPLE_RATE);
}

//! Upward zero crossings per second, over `samples`
double estimateFrequency(const std::vector<float>& samples)
{
    int crossings = 0;
    for (size_t i = 1; i < samples.size(); ++i) {
        if (samples[i - 1] < 0.f && samples[i] >= 0.f) {
            ++crossings;
        }
    }
    return crossings * static_cast<double>(TEST_SAMPLE_RATE) / samples.size();
}

double rms(const std::vector<float>& samples)
{
    double sum = 0.0;
    for (const float s : samples) {
        sum += s * s;
    }
    return std::sqrt(sum / samples.size());
}
}

namespace au::project {
class ClipStretchMethodTests : public ::testing::Test
{
protected:
    QTemporaryDir m_directory;
    au3::Au3ProjectAccessor m_accessor { au::testutils::makeTestContext() };
    StretchMethod m_originalDefault = WaveClip::GetDefaultStretchMethod();

    ::AudacityProject& project()
    {
        return *reinterpret_cast<::AudacityProject*>(m_accessor.au3ProjectPtr());
    }

    QString projectPath() const { return m_directory.filePath("stretch.aup4"); }

    void SetUp() override
    {
        ASSERT_TRUE(QFile::copy(QString::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup4", projectPath()));
        ASSERT_TRUE(m_accessor.load(projectPath().toStdString(), false));
    }

    void TearDown() override
    {
        WaveClip::SetDefaultStretchMethod(m_originalDefault);
        m_accessor.close();
    }

    void loadXml(const wxString& clipAttributes)
    {
        XMLFileReader reader;
        ASSERT_TRUE(reader.ParseString(&ProjectFileIO::Get(project()), projectXml(clipAttributes)));
    }

    WaveTrack& firstTrack()
    {
        return **TrackList::Get(project()).Any<WaveTrack>().begin();
    }

    static WaveClip& onlyClip(WaveTrack& track)
    {
        const auto clips = track.SortedIntervalArray();
        EXPECT_EQ(clips.size(), 1);
        return *clips.front();
    }

    //! A mono track with one second of sine at `TEST_FREQUENCY`
    std::shared_ptr<WaveTrack> sineTrack()
    {
        const auto track = WaveTrackFactory::Get(project()).Create(floatSample, TEST_SAMPLE_RATE);
        std::vector<float> sine(TEST_SAMPLE_RATE);
        for (size_t i = 0; i < sine.size(); ++i) {
            sine[i] = 0.5f * static_cast<float>(std::sin(2.0 * 3.14159265358979 * TEST_FREQUENCY * i / TEST_SAMPLE_RATE));
        }
        track->Append(0, reinterpret_cast<constSamplePtr>(sine.data()), floatSample, sine.size());
        track->Flush();
        TrackList::Get(project()).Add(track);
        return track;
    }

    static std::vector<float> samples(const WaveTrack& track, double t0, double t1)
    {
        const auto start = track.TimeToLongSamples(t0);
        const auto len = (track.TimeToLongSamples(t1) - start).as_size_t();
        std::vector<float> buffer(len);
        EXPECT_TRUE((*track.Channels().begin())->GetFloats(buffer.data(), start, len));
        return buffer;
    }
};

TEST_F(ClipStretchMethodTests, ClipsSavedWithoutAMethodUseStaffPad)
{
    //! [GIVEN] new clips would get another method
    WaveClip::SetDefaultStretchMethod(StretchMethod::Varispeed);

    //! [WHEN] a clip saved before there was a choice is loaded
    loadXml("");

    //! [THEN] it keeps sounding as it did
    EXPECT_EQ(onlyClip(firstTrack()).GetStretchMethod(), StretchMethod::StaffPad);
}

TEST_F(ClipStretchMethodTests, MethodIsLoaded)
{
    loadXml("stretchMethod=\"3\"");
    EXPECT_EQ(onlyClip(firstTrack()).GetStretchMethod(), StretchMethod::Paulstretch);
}

TEST_F(ClipStretchMethodTests, UnknownMethodFallsBackToStaffPad)
{
    loadXml("stretchMethod=\"99\"");
    EXPECT_EQ(onlyClip(firstTrack()).GetStretchMethod(), StretchMethod::StaffPad);
}

TEST_F(ClipStretchMethodTests, MethodIsSaved)
{
    loadXml("");
    onlyClip(firstTrack()).SetStretchMethod(StretchMethod::Varispeed);

    XMLStringWriter writer;
    onlyClip(firstTrack()).WriteXML(0, writer);
    EXPECT_NE(writer.Find("stretchMethod=\"4\""), wxNOT_FOUND) << writer;
}

TEST_F(ClipStretchMethodTests, NewClipsGetTheDefaultAndCopiesKeepTheirOwn)
{
    //! [GIVEN] a default method
    WaveClip::SetDefaultStretchMethod(StretchMethod::Sbsms);

    //! [WHEN] a clip is made
    const auto track = sineTrack();
    WaveClip& clip = onlyClip(*track);

    //! [THEN] it has the default
    EXPECT_EQ(clip.GetStretchMethod(), StretchMethod::Sbsms);

    //! [WHEN] the default changes, and the track is copied
    WaveClip::SetDefaultStretchMethod(StretchMethod::SoundTouch);
    const auto copy = std::static_pointer_cast<WaveTrack>(track->Duplicate());

    //! [THEN] the copied clip keeps its original's method
    EXPECT_EQ(onlyClip(*copy).GetStretchMethod(), StretchMethod::Sbsms);
}

TEST_F(ClipStretchMethodTests, StretchedClipsOfDifferentMethodsAreNotEqual)
{
    const auto track = sineTrack();
    WaveClip& clip = onlyClip(*track);
    const auto copy = std::static_pointer_cast<WaveTrack>(track->Duplicate());
    WaveClip& other = onlyClip(*copy);
    clip.SetStretchMethod(StretchMethod::StaffPad);
    other.SetStretchMethod(StretchMethod::Varispeed);

    //! [THEN] unstretched, the method makes no difference
    EXPECT_TRUE(clip.HasEqualPitchAndSpeed(other));

    //! [THEN] stretched alike, it does
    clip.StretchBy(2.0);
    other.StretchBy(2.0);
    EXPECT_FALSE(clip.HasEqualPitchAndSpeed(other));
}

struct RenderCase {
    StretchMethod method;
    //! The pitch of the sine, once stretched twice as long
    double expectedFrequency;
};

class ClipStretchMethodRenderTests : public ClipStretchMethodTests, public ::testing::WithParamInterface<RenderCase>
{
};

TEST_P(ClipStretchMethodRenderTests, RenderingStretchesWithTheClipsMethod)
{
    //! [GIVEN] a one-second sine clip stretched to two seconds, with the method
    const auto track = sineTrack();
    WaveClip& clip = onlyClip(*track);
    clip.SetStretchMethod(GetParam().method);
    clip.StretchBy(2.0);

    //! [WHEN] its stretching is rendered
    track->ApplyPitchAndSpeed(std::nullopt, {});

    //! [THEN] the rendered clip is two seconds long, not silent, and at the expected pitch
    const WaveClip& rendered = onlyClip(*track);
    ASSERT_FALSE(rendered.HasPitchOrSpeed());
    EXPECT_NEAR(rendered.GetPlayDuration(), 2.0, 1e-3);

    // Away from the edges, where the stretchers settle in
    const auto middle = samples(*track, 0.5, 1.5);
    EXPECT_GT(rms(middle), 0.1);
    EXPECT_NEAR(estimateFrequency(middle), GetParam().expectedFrequency, 0.1 * GetParam().expectedFrequency);
}

INSTANTIATE_TEST_SUITE_P(
    AllMethods, ClipStretchMethodRenderTests,
    ::testing::Values(
        RenderCase { StretchMethod::StaffPad, TEST_FREQUENCY },
        RenderCase { StretchMethod::SoundTouch, TEST_FREQUENCY },
        RenderCase { StretchMethod::Sbsms, TEST_FREQUENCY },
        RenderCase { StretchMethod::Paulstretch, TEST_FREQUENCY },
        // Twice as long, an octave down
        RenderCase { StretchMethod::Varispeed, TEST_FREQUENCY / 2 }),
    [](const ::testing::TestParamInfo<RenderCase>& info) {
    switch (info.param.method) {
    case StretchMethod::StaffPad: return std::string("StaffPad");
    case StretchMethod::SoundTouch: return std::string("SoundTouch");
    case StretchMethod::Sbsms: return std::string("Sbsms");
    case StretchMethod::Paulstretch: return std::string("Paulstretch");
    case StretchMethod::Varispeed: return std::string("Varispeed");
    }
    return std::string("Unknown");
});
}
