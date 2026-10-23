/*
* Audacity: A Digital Audio Editor
*/

#include <gtest/gtest.h>
#include <QFile>
#include <QTemporaryDir>

#include "au3wrap/internal/au3project.h"
#include "au3-project/Project.h"
#include "au3-project-file-io/ProjectFileIO.h"
#include "au3-stretching-sequence/ClipFades.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-xml/XMLFileReader.h"
#include "au3-xml/XMLWriter.h"
#include "testing/testcontext.h"

namespace {
constexpr auto TEST_SAMPLE_RATE = 48000;
constexpr auto TEST_FADE_IN = 0.1;
constexpr auto TEST_FADE_OUT = 0.25;

//! A project with one track holding a one-second clip with fades, the fade-in's type left to its default
wxString projectXml()
{
    return wxString::Format(
        "<project version=\"1.3.0\" audacityversion=\"4.0.0\">"
        "<wavetrack rate=\"%d\" sampleformat=\"262159\" channel=\"0\" linked=\"0\">"
        "<waveclip offset=\"0\" trimLeft=\"0\" trimRight=\"0\" clipStretchRatio=\"1\" fadeIn=\"%.17g\" fadeOut=\"%.17g\" fadeOutType=\"logarithmic\">"
        "<sequence maxsamples=\"262144\" sampleformat=\"262159\" numsamples=\"%d\">"
        "<waveblock start=\"0\" blockid=\"-%d\"/>"
        "</sequence><envelope numpoints=\"0\"/></waveclip>"
        "</wavetrack></project>",
        TEST_SAMPLE_RATE, TEST_FADE_IN, TEST_FADE_OUT, TEST_SAMPLE_RATE,
        TEST_SAMPLE_RATE);
}
}

namespace au::project {
class ClipFadesPersistenceTests : public ::testing::Test
{
protected:
    QTemporaryDir m_directory;
    au3::Au3ProjectAccessor m_accessor { au::testutils::makeTestContext() };

    ::AudacityProject& project()
    {
        return *reinterpret_cast<::AudacityProject*>(m_accessor.au3ProjectPtr());
    }

    QString projectPath() const { return m_directory.filePath("fades.aup4"); }

    void SetUp() override
    {
        ASSERT_TRUE(QFile::copy(QString::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup4", projectPath()));
        ASSERT_TRUE(m_accessor.load(projectPath().toStdString(), false));
        XMLFileReader reader;
        ASSERT_TRUE(reader.ParseString(&ProjectFileIO::Get(project()), projectXml()));
    }

    void TearDown() override
    {
        m_accessor.close();
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
};

TEST_F(ClipFadesPersistenceTests, FadesAreLoaded)
{
    const auto& fades = ClipFades::Get(onlyClip(firstTrack()));
    EXPECT_DOUBLE_EQ(fades.fadeIn(), TEST_FADE_IN);
    EXPECT_DOUBLE_EQ(fades.fadeOut(), TEST_FADE_OUT);
}

TEST_F(ClipFadesPersistenceTests, FadeTypesAreLoadedAndDefaultToLinear)
{
    const auto& fades = ClipFades::Get(onlyClip(firstTrack()));
    // The test XML's first clip only has its fade-out specified as logarithmic.
    EXPECT_EQ(fades.fadeInType(), FadeType::Linear);
    EXPECT_EQ(fades.fadeOutType(), FadeType::Logarithmic);
}

TEST_F(ClipFadesPersistenceTests, FadesAreSaved)
{
    XMLStringWriter writer;
    onlyClip(firstTrack()).WriteXML(0, writer);
    EXPECT_NE(writer.Find(wxString::Format("fadeIn=\"%.8f\"", TEST_FADE_IN)), wxNOT_FOUND) << writer;
    EXPECT_NE(writer.Find(wxString::Format("fadeOut=\"%.8f\"", TEST_FADE_OUT)), wxNOT_FOUND) << writer;
    EXPECT_NE(writer.Find("fadeInType=\"linear\""), wxNOT_FOUND) << writer;
    EXPECT_NE(writer.Find("fadeOutType=\"logarithmic\""), wxNOT_FOUND) << writer;
}

TEST_F(ClipFadesPersistenceTests, CopiedClipsKeepTheirFadesAndFollowTheirOwnStretching)
{
    //! [GIVEN] a copy of the track
    const auto copyHolder = std::static_pointer_cast<WaveTrack>(firstTrack().Duplicate());
    auto& copy = *copyHolder;
    WaveClip& original = onlyClip(firstTrack());
    WaveClip& copied = onlyClip(copy);

    //! [THEN] the copied clip has the same fades
    EXPECT_DOUBLE_EQ(ClipFades::Get(copied).fadeIn(), TEST_FADE_IN);
    EXPECT_DOUBLE_EQ(ClipFades::Get(copied).fadeOut(), TEST_FADE_OUT);
    EXPECT_EQ(ClipFades::Get(copied).fadeOutType(), FadeType::Logarithmic);

    //! [WHEN] the copied clip is stretched
    copied.StretchBy(2.0);

    //! [THEN] its fades are stretched, and the original's aren't
    EXPECT_DOUBLE_EQ(ClipFades::Get(copied).fadeIn(), 2 * TEST_FADE_IN);
    EXPECT_DOUBLE_EQ(ClipFades::Get(copied).fadeOut(), 2 * TEST_FADE_OUT);
    EXPECT_DOUBLE_EQ(ClipFades::Get(original).fadeIn(), TEST_FADE_IN);
    EXPECT_DOUBLE_EQ(ClipFades::Get(original).fadeOut(), TEST_FADE_OUT);

    //! [WHEN] the original clip is stretched
    original.StretchBy(0.5);

    //! [THEN] only its own fades follow
    EXPECT_DOUBLE_EQ(ClipFades::Get(original).fadeIn(), 0.5 * TEST_FADE_IN);
    EXPECT_DOUBLE_EQ(ClipFades::Get(copied).fadeIn(), 2 * TEST_FADE_IN);
}
}

namespace au::project {
TEST_F(ClipFadesPersistenceTests, WinningFadeLongerThanTheTrimmedClipLeavesNoRoomForTheOther)
{
    //! [GIVEN] a one-second clip whose fade-in, set last, takes most of it
    WaveClip& clip = onlyClip(firstTrack());
    auto& fades = ClipFades::Get(clip);
    fades.setFadeOut(0.1);
    fades.setFadeIn(0.8);

    //! [WHEN] the clip is trimmed shorter than its fade-in
    clip.TrimRight(0.5);

    //! [THEN] the fade-in spans the whole clip, and the fade-out vanishes
    EXPECT_DOUBLE_EQ(fades.fadeIn(), 0.5);
    EXPECT_DOUBLE_EQ(fades.fadeOut(), 0.0);
}
}
