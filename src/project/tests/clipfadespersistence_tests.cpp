/*
 * Audacity: A Digital Audio Editor
 */

#include <array>

#include <gtest/gtest.h>
#include <QFile>
#include <QTemporaryDir>

#include "au3wrap/internal/au3project.h"
#include "au3-project/Project.h"
#include "au3-project-file-io/ProjectFileIO.h"
#include "au3-track/TimeWarper.h"
#include "au3-clip-processing/ClipFades.h"
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
        "<waveclip offset=\"0\" trimLeft=\"0\" trimRight=\"0\" clipStretchRatio=\"1\" fadeIn=\"%.17g\" fadeOut=\"%.17g\" fadeOutType=\"1\">"
        "<sequence maxsamples=\"262144\" sampleformat=\"262159\" numsamples=\"%d\">"
        "<waveblock start=\"0\" blockid=\"-%d\"/>"
        "</sequence><envelope numpoints=\"0\"/></waveclip>"
        "</wavetrack></project>",
        TEST_SAMPLE_RATE, TEST_FADE_IN, TEST_FADE_OUT, TEST_SAMPLE_RATE,
        TEST_SAMPLE_RATE);
}

void setFades(WaveClip& clip, double fadeIn, double fadeOut, FadeType fadeOutType = FadeType::Linear)
{
    auto& fades = ClipFades::Get(clip);
    fades.setFadeIn(fadeIn);
    fades.setFadeOutType(fadeOutType);
    fades.setFadeOut(fadeOut);
}

//! The fades either side of the split lines of `splitInThreeWithInnerFades`, left to right:
//! fade-out of the first clip, fade-in and fade-out of the second, fade-in of the third
constexpr std::array<double, 4> INNER_FADES { 0.05, 0.04, 0.06, 0.07 };
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

    //! Some silence to paste, as the effects' processed audio
    std::shared_ptr<WaveTrack> processedAudio(double duration)
    {
        const auto processed = WaveTrackFactory::Get(project()).Create(floatSample, TEST_SAMPLE_RATE);
        processed->InsertSilence(0.0, duration);
        processed->Flush();
        return processed;
    }

    //! Splits the clip at 0.3 and 0.6, with `INNER_FADES` on either side of the split lines,
    //! some logarithmic, and the outer fades of the clip
    void splitInThreeWithInnerFades()
    {
        WaveTrack& track = firstTrack();
        ASSERT_TRUE(track.SplitAt(0.3).first);
        ASSERT_TRUE(track.SplitAt(0.6).first);
        const auto clips = track.SortedIntervalArray();
        ASSERT_EQ(clips.size(), 3);
        setFades(*clips[0], TEST_FADE_IN, INNER_FADES[0]);
        setFades(*clips[1], INNER_FADES[1], INNER_FADES[2], FadeType::Logarithmic);
        setFades(*clips[2], INNER_FADES[3], TEST_FADE_OUT, FadeType::Logarithmic);
    }

    //! Three clips split at `firstSplit` and `secondSplit` and ending at `end`, with the fades of `splitInThreeWithInnerFades`
    void expectSplitInThreeWithInnerFades(double firstSplit, double secondSplit, double end)
    {
        const auto clips = firstTrack().SortedIntervalArray();
        ASSERT_EQ(clips.size(), 3);
        EXPECT_NEAR(clips[0]->GetPlayEndTime(), firstSplit, 1e-9);
        EXPECT_NEAR(clips[1]->GetPlayEndTime(), secondSplit, 1e-9);
        EXPECT_NEAR(clips[2]->GetPlayEndTime(), end, 1e-9);
        const auto& first = ClipFades::Get(*clips[0]);
        const auto& second = ClipFades::Get(*clips[1]);
        const auto& third = ClipFades::Get(*clips[2]);
        EXPECT_DOUBLE_EQ(first.fadeIn(), TEST_FADE_IN);
        EXPECT_DOUBLE_EQ(first.fadeOut(), INNER_FADES[0]);
        EXPECT_DOUBLE_EQ(second.fadeIn(), INNER_FADES[1]);
        EXPECT_DOUBLE_EQ(second.fadeOut(), INNER_FADES[2]);
        EXPECT_EQ(second.fadeOutType(), FadeType::Logarithmic);
        EXPECT_DOUBLE_EQ(third.fadeIn(), INNER_FADES[3]);
        EXPECT_DOUBLE_EQ(third.fadeOut(), TEST_FADE_OUT);
        EXPECT_EQ(third.fadeOutType(), FadeType::Logarithmic);
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
    EXPECT_NE(writer.Find("fadeInType=\"0\""), wxNOT_FOUND) << writer;
    EXPECT_NE(writer.Find("fadeOutType=\"1\""), wxNOT_FOUND) << writer;
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

TEST_F(ClipFadesPersistenceTests, RenderingPitchAndSpeedKeepsTheFades)
{
    //! [GIVEN] the clip stretched, with its fades
    constexpr auto stretchRatio = 2.0;
    onlyClip(firstTrack()).StretchBy(stretchRatio);

    //! [WHEN] its stretching is rendered
    firstTrack().ApplyPitchAndSpeed(std::nullopt, {});

    //! [THEN] the rendered clip has the same fades
    const auto& rendered = onlyClip(firstTrack());
    ASSERT_FALSE(rendered.HasPitchOrSpeed());
    EXPECT_DOUBLE_EQ(ClipFades::Get(rendered).fadeIn(), stretchRatio * TEST_FADE_IN);
    EXPECT_DOUBLE_EQ(ClipFades::Get(rendered).fadeOut(), stretchRatio * TEST_FADE_OUT);
    EXPECT_EQ(ClipFades::Get(rendered).fadeOutType(), FadeType::Logarithmic);
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

TEST_F(ClipFadesPersistenceTests, SplittingInAFadeLeavesItToTheClipWithItsEdge)
{
    //! [WHEN] the clip is split halfway through its fade-in
    const auto [left, right] = firstTrack().SplitAt(TEST_FADE_IN / 2);
    ASSERT_TRUE(left && right);

    //! [THEN] the left clip is faded in all the way, and doesn't fade out
    EXPECT_DOUBLE_EQ(ClipFades::Get(*left).fadeIn(), left->GetPlayDuration());
    EXPECT_DOUBLE_EQ(ClipFades::Get(*left).fadeOut(), 0.0);

    //! [THEN] the right clip isn't faded in, and keeps the fade-out
    EXPECT_DOUBLE_EQ(ClipFades::Get(*right).fadeIn(), 0.0);
    EXPECT_DOUBLE_EQ(ClipFades::Get(*right).fadeOut(), TEST_FADE_OUT);
}

TEST_F(ClipFadesPersistenceTests, SplitDeletingPartOfAFadeLeavesItToTheClipWithItsEdge)
{
    //! [WHEN] the middle of the fade-in is deleted
    firstTrack().SplitDelete(TEST_FADE_IN / 4, TEST_FADE_IN / 2);

    const auto clips = firstTrack().SortedIntervalArray();
    ASSERT_EQ(clips.size(), 2);
    const auto& left = *clips.front();
    const auto& right = *clips.back();

    //! [THEN] the left clip is faded in all the way, and doesn't fade out
    EXPECT_DOUBLE_EQ(ClipFades::Get(left).fadeIn(), left.GetPlayDuration());
    EXPECT_DOUBLE_EQ(ClipFades::Get(left).fadeOut(), 0.0);

    //! [THEN] the right clip isn't faded in, and keeps the fade-out
    EXPECT_DOUBLE_EQ(ClipFades::Get(right).fadeIn(), 0.0);
    EXPECT_DOUBLE_EQ(ClipFades::Get(right).fadeOut(), TEST_FADE_OUT);
}

TEST_F(ClipFadesPersistenceTests, CopyingPartOfAFadeKeepsItOnlyWithItsEdge)
{
    //! [WHEN] copying from the clip's start to the middle of the fade-in
    const auto withEdge = firstTrack().Copy(0, TEST_FADE_IN / 2);
    //! [THEN] the copy is faded in all the way, and doesn't fade out
    const auto& withEdgeClip = onlyClip(*std::static_pointer_cast<WaveTrack>(withEdge));
    EXPECT_DOUBLE_EQ(ClipFades::Get(withEdgeClip).fadeIn(), withEdgeClip.GetPlayDuration());
    EXPECT_DOUBLE_EQ(ClipFades::Get(withEdgeClip).fadeOut(), 0.0);

    //! [WHEN] copying from the middle of the fade-in
    const auto withoutEdge = firstTrack().Copy(TEST_FADE_IN / 2, TEST_FADE_IN);
    //! [THEN] the copy has no fades
    const auto& withoutEdgeClip = onlyClip(*std::static_pointer_cast<WaveTrack>(withoutEdge));
    EXPECT_DOUBLE_EQ(ClipFades::Get(withoutEdgeClip).fadeIn(), 0.0);
    EXPECT_DOUBLE_EQ(ClipFades::Get(withoutEdgeClip).fadeOut(), 0.0);
}

//! `WaveTrack::ClearAndPaste` is how Equalization, Noise Reduction, Change Tempo, Change Pitch, Paulstretch,
//! the Nyquist effects and the generators write their result back: the region is cleared, and the
//! processed audio, in a track of its own, is pasted in its place.
TEST_F(ClipFadesPersistenceTests, ClearAndPasteOverTheWholeClipKeepsItsFades)
{
    //! [GIVEN] processed audio for the whole clip
    WaveTrack& track = firstTrack();
    const WaveClip& clip = onlyClip(track);
    const auto t0 = clip.GetPlayStartTime();
    const auto t1 = clip.GetPlayEndTime();
    const auto processed = processedAudio(t1 - t0);

    //! [WHEN] it replaces the clip's audio, as the effects do
    constexpr auto preserveSplits = true;
    constexpr auto merge = true;
    track.ClearAndPaste(t0, t1, *processed, preserveSplits, merge);

    //! [THEN] the clip still has its fades
    const auto& fades = ClipFades::Get(onlyClip(track));
    EXPECT_DOUBLE_EQ(fades.fadeIn(), TEST_FADE_IN);
    EXPECT_DOUBLE_EQ(fades.fadeOut(), TEST_FADE_OUT);
}

TEST_F(ClipFadesPersistenceTests, ClearAndPasteBetweenContiguousClipsKeepsTheirFades)
{
    //! [GIVEN] the clip split in three: the first keeps the fade-in, the last keeps the fade-out
    WaveTrack& track = firstTrack();
    constexpr auto t0 = 0.3;
    constexpr auto t1 = 0.6;
    ASSERT_TRUE(track.SplitAt(t0).first);
    ASSERT_TRUE(track.SplitAt(t1).first);
    ASSERT_EQ(track.SortedIntervalArray().size(), 3);
    ASSERT_DOUBLE_EQ(ClipFades::Get(*track.SortedIntervalArray().front()).fadeIn(), TEST_FADE_IN);
    ASSERT_DOUBLE_EQ(ClipFades::Get(*track.SortedIntervalArray().back()).fadeOut(), TEST_FADE_OUT);

    //! [WHEN] the middle clip's audio is replaced, as the effects do
    const auto processed = processedAudio(t1 - t0);
    constexpr auto preserveSplits = true;
    constexpr auto merge = true;
    track.ClearAndPaste(t0, t1, *processed, preserveSplits, merge);

    //! [THEN] whatever clips remain, the track still fades in at its start and out at its end
    const auto clips = track.SortedIntervalArray();
    ASSERT_FALSE(clips.empty());
    EXPECT_DOUBLE_EQ(ClipFades::Get(*clips.front()).fadeIn(), TEST_FADE_IN);
    EXPECT_DOUBLE_EQ(ClipFades::Get(*clips.back()).fadeOut(), TEST_FADE_OUT);
}

TEST_F(ClipFadesPersistenceTests, ClearAndPasteOverSplitLinesKeepsTheFadesAtTheirEdges)
{
    ASSERT_NO_FATAL_FAILURE(splitInThreeWithInnerFades());

    //! [WHEN] audio across both split lines is replaced, as the effects do
    constexpr auto t0 = 0.2;
    constexpr auto t1 = 0.8;
    constexpr auto preserveSplits = true;
    constexpr auto merge = true;
    firstTrack().ClearAndPaste(t0, t1, *processedAudio(t1 - t0), preserveSplits, merge);

    //! [THEN] the split lines are back, and every edge has its fade back
    expectSplitInThreeWithInnerFades(0.3, 0.6, 1.0);
}

TEST_F(ClipFadesPersistenceTests, ClearAndPasteNotPreservingSplitLinesKeepsOnlyTheOuterFades)
{
    WaveTrack& track = firstTrack();
    ASSERT_NO_FATAL_FAILURE(splitInThreeWithInnerFades());

    //! [WHEN] audio across both split lines is replaced, without preserving them
    constexpr auto t0 = 0.2;
    constexpr auto t1 = 0.8;
    constexpr auto preserveSplits = false;
    constexpr auto merge = true;
    track.ClearAndPaste(t0, t1, *processedAudio(t1 - t0), preserveSplits, merge);

    //! [THEN] one clip remains, with the outer fades only
    const auto& fades = ClipFades::Get(onlyClip(track));
    EXPECT_DOUBLE_EQ(fades.fadeIn(), TEST_FADE_IN);
    EXPECT_DOUBLE_EQ(fades.fadeOut(), TEST_FADE_OUT);
    EXPECT_EQ(fades.fadeOutType(), FadeType::Logarithmic);
}

//! As Change Speed does: the region gets shorter, and what was in it gets closer
TEST_F(ClipFadesPersistenceTests, ClearAndPasteWithTimeWarperMovesTheFadesWithTheirEdges)
{
    ASSERT_NO_FATAL_FAILURE(splitInThreeWithInnerFades());

    //! [WHEN] [0.2, 0.8] is played one and a half times as fast, leaving every clip room for its fades
    constexpr auto t0 = 0.2;
    constexpr auto t1 = 0.8;
    constexpr auto newT1 = 0.6;
    const RegionTimeWarper warper { t0, t1, std::make_unique<LinearTimeWarper>(t0, t0, t1, newT1) };
    constexpr auto preserveSplits = true;
    constexpr auto merge = true;
    firstTrack().ClearAndPaste(t0, t1, *processedAudio(newT1 - t0), preserveSplits, merge, &warper);

    //! [THEN] the split lines are where the warper put them, the end has moved back by as much as the region shortened,
    //! and the fades have followed
    constexpr auto scale = (newT1 - t0) / (t1 - t0);
    expectSplitInThreeWithInnerFades(t0 + (0.3 - t0) * scale, t0 + (0.6 - t0) * scale, 1.0 - (t1 - newT1));
}
}
