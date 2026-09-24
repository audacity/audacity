/*
* Audacity: A Digital Audio Editor
*/

#include <gtest/gtest.h>
#include <QFile>
#include <QTemporaryDir>

#include <optional>

#include "au3wrap/internal/au3project.h"
#include "au3-mixer/Envelope.h"
#include "au3-project/Project.h"
#include "au3-project-file-io/ProjectFileIO.h"
#include "au3-stretching-sequence/TempoChange.h"
#include "au3-wave-track/TimeStretching.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-wave-track/WaveTrackUtilities.h"
#include "au3-xml/XMLFileReader.h"
#include "testing/testcontext.h"

namespace {
//! TimeStretching.cpp registers the `OnProjectTempoChange` override for
//! `WaveTrack`. Nothing else in this binary references that translation unit,
//! so without this the linker drops it and `DoProjectTempoChange` silently
//! does nothing.
[[maybe_unused]] const auto& forceTimeStretchingLink
    =TimeStretching::defaultStretchRenderingTitle;

// Default test values - may be overwritten.
constexpr auto TEST_PROJECT_TEMPO = 188.0;
constexpr auto TEST_SAMPLE_RATE = 48000; // Applies to the track and its clips
constexpr auto TEST_CLIP_RAW_AUDIO_TEMPO = 130.0;
constexpr auto TEST_CLIP_LEFT_TRIM = 0.2;
constexpr auto TEST_CLIP_RIGHT_TRIM = 0.3;
// This stretch ratio would be obtained if the user imported a loop at
// `RAW_AUDIO_TEMPO`, set the project tempo to `PROJECT_TEMPO` and then
// reset clip pitch and speed.
constexpr auto TEST_CLIP_STRETCH_RATIO = TEST_PROJECT_TEMPO / TEST_CLIP_RAW_AUDIO_TEMPO;

struct ClipXml
{
    std::optional<double> rawAudioTempo = TEST_CLIP_RAW_AUDIO_TEMPO;
    std::optional<double> clipTempo;
    std::optional<bool> clipStretchToMatchTempo;
    double clipStretchRatio = TEST_CLIP_STRETCH_RATIO;
    const char* envelope = "<envelope numpoints=\"0\"/>";

    ClipXml& withRawAudioTempo(std::optional<double> tempo) { rawAudioTempo = tempo; return *this; }
    ClipXml& withClipTempo(double tempo) { clipTempo = tempo; return *this; }
    ClipXml& withClipStretchToMatchTempo(bool matching) { clipStretchToMatchTempo = matching; return *this; }
    ClipXml& withClipStretchRatio(double ratio) { clipStretchRatio = ratio; return *this; }
    ClipXml& withEnvelope(const char* xml) { envelope = xml; return *this; }
};

wxString optionalAttribute(const char* name, const std::optional<double>& value)
{
    return value.has_value() ? wxString::Format("%s=\"%.17g\" ", name, *value) : wxString {};
}

wxString optionalAttribute(const char* name, const std::optional<bool>& value)
{
    return value.has_value() ? wxString::Format("%s=\"%d\" ", name, static_cast<int>(*value)) : wxString {};
}

// The XML of a project at `TEST_PROJECT_TEMPO`.
// Wrongly importing a clip's net stretch ratio can lead to overlapping clips,
// which we want to test, too, hence the two clips. These are identical,
// with a fixed 0.5s offset for the second one. Each one lasts one second,
// trimmed by `TEST_CLIP_LEFT_TRIM` and `TEST_CLIP_RIGHT_TRIM`.
// `stereo` writes them as the linked pair of channel tracks a stereo track is
// stored as.
wxString projectXml(const ClipXml& clip, bool stereo = false)
{
    wxString tracks;
    for (int channel = 0; channel < (stereo ? 2 : 1); ++channel) {
        tracks += wxString::Format(
            "<wavetrack rate=\"%d\" sampleformat=\"262159\" channel=\"%d\" linked=\"%d\">",
            TEST_SAMPLE_RATE, channel, stereo && channel == 0 ? 3 : 0);
        for (const auto offset : { 0.0, 0.5 }) {
            tracks += wxString::Format(
                "<waveclip offset=\"%.17g\" trimLeft=\"%.17g\" trimRight=\"%.17g\" clipStretchRatio=\"%.17g\" %s%s%s>"
                "<sequence maxsamples=\"262144\" sampleformat=\"262159\" numsamples=\"%d\">"
                "<waveblock start=\"0\" blockid=\"-%d\"/>"
                "</sequence>%s</waveclip>",
                offset, TEST_CLIP_LEFT_TRIM, TEST_CLIP_RIGHT_TRIM, clip.clipStretchRatio,
                optionalAttribute("rawAudioTempo", clip.rawAudioTempo),
                optionalAttribute("clipTempo", clip.clipTempo),
                optionalAttribute("clipStretchToMatchTempo", clip.clipStretchToMatchTempo),
                TEST_SAMPLE_RATE, TEST_SAMPLE_RATE, clip.envelope);
        }
        tracks += "</wavetrack>";
    }
    return wxString::Format(
        "<project version=\"1.3.0\" audacityversion=\"3.7.9\" time_signature_tempo=\"%.17g\">%s</project>",
        TEST_PROJECT_TEMPO, tracks);
}

//! What a clip serializes back to.
wxString writtenXml(const WaveClip& clip)
{
    XMLStringWriter writer;
    clip.WriteXML(0, writer);
    return writer;
}
}

namespace au::project {
class ClipTempoPersistenceTests : public ::testing::Test
{
protected:
    QTemporaryDir m_directory;
    au3::Au3ProjectAccessor m_accessor { au::testutils::makeTestContext() };

    ::AudacityProject& project()
    {
        return *reinterpret_cast<::AudacityProject*>(m_accessor.au3ProjectPtr());
    }

    QString projectPath() const { return m_directory.filePath("tempo.aup4"); }

    void SetUp() override
    {
        ASSERT_TRUE(QFile::copy(QString::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup4", projectPath()));
        ASSERT_TRUE(m_accessor.load(projectPath().toStdString(), false));
    }

    void TearDown() override
    {
        m_accessor.close();
    }

    //! Deserializes `xml` into the test project pointer.
    [[nodiscard]] bool readProject(const wxString& xml)
    {
        XMLFileReader reader;
        return reader.ParseString(&ProjectFileIO::Get(project()), xml);
    }

    WaveTrack& firstTrack()
    {
        return **TrackList::Get(project()).Any<WaveTrack>().begin();
    }

    WaveTrack::IntervalHolders clips()
    {
        return firstTrack().SortedIntervalArray();
    }

    void expectStretchRatio(double stretchRatio)
    {
        const auto intervals = clips();
        ASSERT_EQ(intervals.size(), 2);
        for (const auto& clip : intervals) {
            EXPECT_DOUBLE_EQ(clip->GetStretchRatio(), stretchRatio);
        }
    }
};

TEST_F(ClipTempoPersistenceTests, LegacyClipsKeepTheirSpeed)
{
    //! [GIVEN] the default legacy test XML file
    const wxString xml = projectXml({});

    //! [WHEN] the file is deserialized
    ASSERT_TRUE(readProject(xml));

    //! [THEN] stretch ratio of its clips are as expected
    expectStretchRatio(1.0);
}

TEST_F(ClipTempoPersistenceTests, RemovingOverlapOnProjectLoadDoesNotUndulyTrimClips)
{
    // There used to be a bug that resulted in clips overlapping, which we've since fixed
    // but may still be present in legacy projects. Hence, our project-loading steps include
    // a remove-overlap step.
    // If a clip's net stretch ratio isn't restored properly from a legacy project, it can be
    // that it now largely overlaps another clip, which would result in undue trimming.

    //! [GIVEN] a deserialized legacy project
    ASSERT_TRUE(readProject(projectXml({})));

    //! [WHEN] the loader resolves overlapping clips
    WaveTrackUtilities::RemoveOverlaps(firstTrack());

    //! [THEN] it finds none, and trims nothing away
    for (const auto& clip : clips()) {
        EXPECT_DOUBLE_EQ(clip->GetTrimRight(), TEST_CLIP_RIGHT_TRIM);
    }
}

TEST_F(ClipTempoPersistenceTests, ProjectTempoChangeStretchesImportedClips)
{
    //! [GIVEN] a deserialized legacy project
    ASSERT_TRUE(readProject(projectXml({})));

    //! [WHEN] the project tempo is halved
    DoProjectTempoChange(firstTrack(), TEST_PROJECT_TEMPO / 2);

    //! [THEN] its clips play twice as slowly
    expectStretchRatio(2 * 1.0);
}

TEST_F(ClipTempoPersistenceTests, LegacyStereoClipsKeepSpeedWhenChannelsAreJoined)
{
    // Historically, stereo tracks were implemented as two mono tracks with a
    // hierarchy, the "leader" track and its follower. `LinkConsistencyFix` was
    // engineered to convert such dual-mono representation to nowadays' real stereo
    // track.

    //! [GIVEN] a legacy project XML file storing a stereo track as a linked
    //! pair of channel tracks
    const wxString xml = projectXml({}, /*stereo=*/ true);

    //! [WHEN] the file is deserialized and the channels are joined
    ASSERT_TRUE(readProject(xml));
    firstTrack().LinkConsistencyFix(true);

    //! [THEN] one stereo track comes out, its clips unstretched
    ASSERT_EQ(firstTrack().NChannels(), 2);
    expectStretchRatio(1.0);
}

TEST_F(ClipTempoPersistenceTests, ExplicitClipTempoIsPreservedStretchToTempoChangesIsDisabled)
{
    // In 4.0, we added the possibility of decoupling a clip's automatic stretch on project tempo change
    // (the "Stretch to tempo changes" option, ON by default).

    //! [GIVEN] a project XML file whose clips are pinned to 150 bpm
    const wxString xml = projectXml(ClipXml {}.withClipTempo(150.0).withClipStretchToMatchTempo(false));

    //! [WHEN] the file is deserialized
    ASSERT_TRUE(readProject(xml));

    //! [THEN] its clips play at their own tempo, not the project's
    EXPECT_FALSE(clips().front()->GetStretchToMatchProjectTempo());
    expectStretchRatio(TEST_PROJECT_TEMPO / 150.0);

    //! [WHEN] the project tempo changes
    DoProjectTempoChange(firstTrack(), TEST_PROJECT_TEMPO / 2);

    //! [THEN] they are left alone
    expectStretchRatio(TEST_PROJECT_TEMPO / 150.0);
}

TEST_F(ClipTempoPersistenceTests, MissingClipTempoIsPreservedWhenStretchToTempoChangesIsDisabled)
{
    // Normally not a use case: Disabling "Stretch to tempo changes" should have set the `clipTempo` property.
    // Still, if it happens that a project file is not to match project tempo but doesn't have a clipTempo either,
    // the sensible thing to do is probably to fall back on the stretch ratio caused by the user stretching the clip.

    //! [GIVEN] a project XML file whose clips follow no tempo and name none
    const wxString xml = projectXml(ClipXml {}.withClipStretchToMatchTempo(false));

    //! [WHEN] the file is deserialized
    ASSERT_TRUE(readProject(xml));

    //! [THEN] its clips keep exactly the stretch the file gave them, and none
    //! is invented on the way back out
    expectStretchRatio(TEST_CLIP_STRETCH_RATIO);
    EXPECT_FALSE(writtenXml(*clips().front()).Contains("clipTempo="));
}

TEST_F(ClipTempoPersistenceTests, MissingRawTempoIsNotInferredFromProjectTempo)
{
    //! [GIVEN] a project XML file whose clips say nothing about the tempo of
    //! the audio they hold
    const wxString xml = projectXml(ClipXml {}.withRawAudioTempo(std::nullopt)
                                    .withClipTempo(150.0).withClipStretchToMatchTempo(false));

    //! [WHEN] the file is deserialized
    ASSERT_TRUE(readProject(xml));

    //! [THEN] no tempo mapping applies, and the project's tempo is not taken
    //! for the audio's own
    expectStretchRatio(TEST_CLIP_STRETCH_RATIO);
    EXPECT_FALSE(writtenXml(*clips().front()).Contains("rawAudioTempo="));
}

TEST_F(ClipTempoPersistenceTests, LegacySlowedClipsKeepEnvelopePointsBeyondRawDuration)
{
    // Clip envelopes don't have an `mStretchRatio` member: all its time points are scaled destructively
    // upon tempo change. Hence, a clip of raw duration 1s in a project at lower speed may have envelope points
    // beyond 1s - `<controlpoint t=\"1.1\" val=\"0.2\"/>`.
    // When importing, its envelope should remain as saved.

    //! [GIVEN] a legacy project XML file whose clips were automatically slowed down to match project tempo,
    //! with an envelope point past the end of the raw audio
    const wxString xml = projectXml(ClipXml {}.withRawAudioTempo(TEST_PROJECT_TEMPO * 1.5).withClipStretchRatio(1.0)
                                    .withEnvelope("<envelope numpoints=\"2\">"
                                                  "<controlpoint t=\"0\" val=\"0.8\"/>"
                                                  "<controlpoint t=\"1.1\" val=\"0.2\"/>"
                                                  "</envelope>"));

    //! [WHEN] the file is deserialized
    ASSERT_TRUE(readProject(xml));

    //! [THEN] the envelope is restored as it was saved.
    expectStretchRatio(1.5);
    for (const auto& clip : clips()) {
        const auto& envelope = clip->GetEnvelope();
        EXPECT_DOUBLE_EQ(envelope.GetTrackLen(), 1.5);
        ASSERT_EQ(envelope.GetNumberOfPoints(), 2);
        double times[2], values[2];
        envelope.GetPoints(times, values, 2);
        EXPECT_DOUBLE_EQ(times[0] + envelope.GetOffset(), 0.0);
        EXPECT_DOUBLE_EQ(times[1] + envelope.GetOffset(), 1.1);
    }
}

TEST_F(ClipTempoPersistenceTests, InheritedTempoSurvivesCopiesAndPaste)
{
    //! [GIVEN] a deserialized legacy project
    ASSERT_TRUE(readProject(projectXml({})));
    const auto clip = clips().front();
    const auto& factory = WaveTrackFactory::Get(project()).GetSampleBlockFactory();

    //! [WHEN] its first clip is copied whole, cut down to a range, and pasted
    //! into an empty clip
    // `copyCutlines` and `backup` values shouldn't matter.
    auto fullCopy = WaveClip::NewSharedFrom(*clip, factory, /*copyCutlines=*/ true, /*backup=*/ true);
    // `NewSharedFromRange` keeps everything, only trims left and right as needed.
    auto copyFromRange = WaveClip::NewSharedFromRange(*clip, factory, /*copyCutlines=*/ false, /*backup=*/ true, 0.3, 0.6);
    auto pasted = WaveClip::NewShared(1 /*num channels*/, factory, floatSample, TEST_SAMPLE_RATE);
    ASSERT_TRUE(pasted->Paste(0.0, *clip));

    //! [THEN] each one plays like the original, and still follows the project
    //! rather than carrying a tempo of its own
    for (const auto& result : { fullCopy, copyFromRange, pasted }) {
        EXPECT_DOUBLE_EQ(result->GetStretchRatio(), 1.0);
        EXPECT_TRUE(result->GetStretchToMatchProjectTempo());
        EXPECT_FALSE(writtenXml(*result).Contains("clipTempo="));
    }

    //! [THEN] and each covers the stretch of audio it was asked for
    EXPECT_NEAR(copyFromRange->GetPlayStartTime(), 0.3, 1.0 / TEST_SAMPLE_RATE);
    EXPECT_NEAR(copyFromRange->GetPlayEndTime(), 0.6, 1.0 / TEST_SAMPLE_RATE);
    EXPECT_NEAR(pasted->GetPlayEndTime(), 0.5, 1.0 / TEST_SAMPLE_RATE);
}

TEST_F(ClipTempoPersistenceTests, PasteIntoMatchingClipPreservesSpeedOfUnmatchedSource)
{
    //! [GIVEN] a deserialized project whose clips don't follow its tempo, and
    //! an empty clip that does
    const wxString xml = projectXml(ClipXml {}.withClipStretchToMatchTempo(false).withClipStretchRatio(1.0));
    ASSERT_TRUE(readProject(xml));
    auto pasted = WaveClip::NewShared(1, WaveTrackFactory::Get(project()).GetSampleBlockFactory(), floatSample, TEST_SAMPLE_RATE);
    pasted->OnProjectTempoChange(std::nullopt, TEST_PROJECT_TEMPO);

    //! [WHEN] the unmatched clip is pasted into the matching one
    ASSERT_TRUE(pasted->Paste(0.0, *clips().front()));

    //! [THEN] the target keeps following the project, at the source's speed
    EXPECT_TRUE(pasted->GetStretchToMatchProjectTempo());
    EXPECT_DOUBLE_EQ(pasted->GetStretchRatio(), 1.0);
}

TEST_F(ClipTempoPersistenceTests, PasteIntoUnmatchedClipPreservesInheritedSourceSpeed)
{
    //! [GIVEN] a deserialized 4.0 project, and an empty clip that doesn't
    //! follow the project tempo
    ASSERT_TRUE(readProject(projectXml(ClipXml {}.withClipStretchToMatchTempo(true))));
    auto pasted = WaveClip::NewShared(1, WaveTrackFactory::Get(project()).GetSampleBlockFactory(), floatSample, TEST_SAMPLE_RATE);
    pasted->SetStretchToMatchProjectTempo(false);

    //! [WHEN] a matching clip is pasted into it
    ASSERT_TRUE(pasted->Paste(0.0, *clips().front()));

    //! [THEN] the target keeps ignoring the project, at the source's speed
    EXPECT_FALSE(pasted->GetStretchToMatchProjectTempo());
    EXPECT_DOUBLE_EQ(pasted->GetStretchRatio(), 1.0);
}

TEST_F(ClipTempoPersistenceTests, DisablingStretchToTempoChangesFreezesInheritedTempo)
{
    //! [GIVEN] a deserialized legacy project
    ASSERT_TRUE(readProject(projectXml({})));
    const auto clip = clips().front();

    //! [WHEN] a clip stops following the project tempo, and that tempo is halved
    clip->SetStretchToMatchProjectTempo(false);
    ASSERT_DOUBLE_EQ(clip->GetStretchRatio(), 1.0);
    DoProjectTempoChange(firstTrack(), TEST_PROJECT_TEMPO / 2);

    //! [THEN] the clip is left alone, and writes down the tempo it was frozen
    //! at, now that the project's own no longer says what it plays at
    EXPECT_DOUBLE_EQ(clip->GetStretchRatio(), 1.0);
    EXPECT_TRUE(writtenXml(*clip).Contains("clipTempo=\"188"));
}

TEST_F(ClipTempoPersistenceTests, EnablingStretchToTempoChangesPreservesSpeedAfterProjectTempoChanged)
{
    //! [GIVEN] a deserialized project whose clips don't follow its tempo, which
    //! has since been halved
    const wxString xml = projectXml(ClipXml {}.withClipStretchToMatchTempo(false).withClipStretchRatio(1.0));
    ASSERT_TRUE(readProject(xml));
    const auto clip = clips().front();
    DoProjectTempoChange(firstTrack(), TEST_PROJECT_TEMPO / 2);
    ASSERT_DOUBLE_EQ(clip->GetStretchRatio(), 1.0);

    //! [WHEN] a clip starts following the project tempo
    clip->SetStretchToMatchProjectTempo(true);

    //! [THEN] its speed doesn't change under it
    EXPECT_DOUBLE_EQ(clip->GetStretchRatio(), 1.0);
}

TEST_F(ClipTempoPersistenceTests, ProjectTempoChangesUpdateExplicitTempoWhenMatchingIsEnabled)
{
    //! [GIVEN] a deserialized project whose clips name 150 bpm but follow the
    //! project's tempo
    ASSERT_TRUE(readProject(projectXml(ClipXml {}.withClipTempo(150.0))));
    expectStretchRatio(TEST_PROJECT_TEMPO / 150.0);

    //! [WHEN] the project tempo is halved
    DoProjectTempoChange(firstTrack(), TEST_PROJECT_TEMPO / 2);

    //! [THEN] the clips take the new tempo for their own
    expectStretchRatio(2.0);
}

TEST_F(ClipTempoPersistenceTests, SaveAndReloadPreservesInheritedAndFrozenTempo)
{
    //! [GIVEN] a deserialized legacy project, of whose two clips only the first
    //! follows the project tempo
    ASSERT_TRUE(readProject(projectXml({})));
    firstTrack().LinkConsistencyFix(true);
    clips().back()->SetStretchToMatchProjectTempo(false);

    //! [WHEN] the project is saved and read back
    ASSERT_TRUE(m_accessor.save(projectPath().toStdString()));
    m_accessor.close();
    ASSERT_TRUE(m_accessor.load(projectPath().toStdString(), /*ignoreAutosave=*/ false));

    //! [THEN] both clips come back as they were
    const auto reloaded = clips();
    ASSERT_EQ(reloaded.size(), 2);
    EXPECT_TRUE(reloaded.front()->GetStretchToMatchProjectTempo());
    EXPECT_FALSE(reloaded.back()->GetStretchToMatchProjectTempo());
    for (const auto& clip : reloaded) {
        EXPECT_NEAR(clip->GetStretchRatio(), 1.0, 1e-8);
    }
}

struct ClipTempoCase {
    const char* name;
    ClipXml clip;
    double expectedStretchRatio;
};

class ClipTempoDefaultsTests : public ClipTempoPersistenceTests, public ::testing::WithParamInterface<ClipTempoCase>
{
};

TEST_P(ClipTempoDefaultsTests, OnlyTheApplicableTempoIsUsed)
{
    //! [GIVEN] a project XML file with this combination of tempo attributes
    const wxString xml = projectXml(GetParam().clip);

    //! [WHEN] the file is deserialized
    ASSERT_TRUE(readProject(xml));

    //! [THEN] only the tempo that applies is taken into account
    expectStretchRatio(GetParam().expectedStretchRatio);
}

TEST_P(ClipTempoDefaultsTests, TogglingMatchingPreservesTheStretchRatio)
{
    //! [GIVEN] a deserialized project with this combination of tempo attributes
    ASSERT_TRUE(readProject(projectXml(GetParam().clip)));

    //! [WHEN] its clips are made to follow the project tempo, and then not to
    for (const auto& clip : clips()) {
        const auto matching = clip->GetStretchToMatchProjectTempo();
        for (bool enabled : { !matching, matching }) {
            clip->SetStretchToMatchProjectTempo(enabled);

            //! [THEN] neither switch changes how fast they play
            EXPECT_DOUBLE_EQ(clip->GetStretchRatio(), GetParam().expectedStretchRatio);
        }
    }
}

INSTANTIATE_TEST_SUITE_P(SerializedTempo, ClipTempoDefaultsTests, ::testing::Values(
                             ClipTempoCase { "NoTempoMetadata",
                                             ClipXml {}.withRawAudioTempo(std::nullopt).withClipStretchRatio(1.0), 1.0 },
                             ClipTempoCase { "UnknownRawTempo",
                                             ClipXml {}.withRawAudioTempo(0.0).withClipStretchRatio(1.0), 1.0 },
                             ClipTempoCase { "RawTempoMatchesProject",
                                             ClipXml {}.withRawAudioTempo(TEST_PROJECT_TEMPO).withClipStretchRatio(1.0), 1.0 },
                             ClipTempoCase { "FollowProject",
                                             ClipXml {}.withClipStretchToMatchTempo(true).withClipStretchRatio(1.0),
                                             TEST_CLIP_RAW_AUDIO_TEMPO / TEST_PROJECT_TEMPO },
                             ClipTempoCase { "DoNotFollowProject",
                                             ClipXml {}.withClipStretchToMatchTempo(false).withClipStretchRatio(1.0), 1.0 },
                             ClipTempoCase { "ExplicitTempo",
                                             ClipXml {}.withClipTempo(150.0).withClipStretchToMatchTempo(true).withClipStretchRatio(1.0),
                                             TEST_CLIP_RAW_AUDIO_TEMPO / 150.0 },
                             ClipTempoCase { "ExplicitTempoWithoutRawTempo",
                                             ClipXml {}.withRawAudioTempo(std::nullopt).withClipTempo(150.0).withClipStretchRatio(1.0),
                                             1.0 },
                             ClipTempoCase { "ExplicitTempoAfterDisabledMatching",
                                             ClipXml {}.withClipTempo(150.0).withClipStretchToMatchTempo(false).withClipStretchRatio(1.0),
                                             TEST_CLIP_RAW_AUDIO_TEMPO / 150.0 }
                             ), [](const ::testing::TestParamInfo<ClipTempoCase>& info) { return info.param.name; });
}
