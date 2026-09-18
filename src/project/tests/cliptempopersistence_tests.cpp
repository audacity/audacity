/*
* Audacity: A Digital Audio Editor
*/

#include <gtest/gtest.h>
#include <QFile>
#include <QTemporaryDir>

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

    // Exercise the project reader's track-addition notifications and clip XML
    // handlers, using silent blocks so no recorded audio fixture is needed.
    bool readProject(const wxString& clipAttributes = {}, bool stereo = false,
                     const wxString& rawTempoAttribute = "rawAudioTempo=\"130\"", double stretchRatio = 188.0 / 130.0,
                     const wxString& envelope = "<envelope numpoints=\"0\"/>")
    {
        const auto path = m_directory.filePath("tempo.aup4");
        if (!QFile::copy(QString::fromUtf8(au_project_tests_DATA_ROOT) + "/data/empty.aup4", path)
            || !m_accessor.load(path.toStdString(), false)) {
            return false;
        }

        wxString tracks;
        for (int channel = 0; channel < (stereo ? 2 : 1); ++channel) {
            tracks += wxString::Format(
                "<wavetrack rate=\"48000\" sampleformat=\"262159\" channel=\"%d\" linked=\"%d\">",
                channel, stereo && channel == 0 ? 3 : 0);
            for (const auto offset : { "0", "0.5" }) {
                tracks += wxString::Format(
                    "<waveclip offset=\"%s\" trimLeft=\"0.2\" trimRight=\"0.3\" "
                    "%s clipStretchRatio=\"%.17g\" %s>"
                    "<sequence maxsamples=\"262144\" sampleformat=\"262159\" numsamples=\"48000\">"
                    "<waveblock start=\"0\" blockid=\"-48000\"/>"
                    "</sequence>%s</waveclip>", offset, rawTempoAttribute, stretchRatio, clipAttributes, envelope);
            }
            tracks += "</wavetrack>";
        }
        XMLFileReader reader;
        return reader.ParseString(&ProjectFileIO::Get(project()),
                                  "<project version=\"1.3.0\" audacityversion=\"3.7.9\" "
                                  "time_signature_tempo=\"188\">" + tracks + "</project>");
    }

    WaveTrack* firstTrack()
    {
        return *TrackList::Get(project()).Any<WaveTrack>().begin();
    }

    void TearDown() override
    {
        m_accessor.close();
    }
};

TEST_F(ClipTempoPersistenceTests, LegacyClipsKeepSpeedAndTrimmedBoundaries)
{
    ASSERT_TRUE(readProject());
    auto& track = *firstTrack();
    track.LinkConsistencyFix(true);
    const auto clips = track.SortedIntervalArray();
    ASSERT_EQ(clips.size(), 2);

    for (size_t i = 0; i < clips.size(); ++i) {
        EXPECT_DOUBLE_EQ(clips[i]->GetStretchRatio(), 1.0);
        EXPECT_NEAR(clips[i]->GetPlayStartTime(), 0.2 + 0.5 * i, 1e-9);
        EXPECT_NEAR(clips[i]->GetPlayEndTime(), 0.7 + 0.5 * i, 1e-9);
        EXPECT_DOUBLE_EQ(clips[i]->GetTrimLeft(), 0.2);
        EXPECT_DOUBLE_EQ(clips[i]->GetTrimRight(), 0.3);
        EXPECT_DOUBLE_EQ(clips[i]->GetEnvelope().GetTrackLen(), 1.0);
    }

    // Loading must not mistake the inflated legacy durations for overlaps.
    WaveTrackUtilities::RemoveOverlaps(track);
    for (const auto& clip : clips) {
        EXPECT_NEAR(clip->GetTrimRight(), 0.3, 1e-9);
        EXPECT_TRUE(TimeStretching::SetClipStretchRatio(*clip, 1.0));
        EXPECT_NEAR(clip->GetPlayEndTime() - clip->GetPlayStartTime(), 0.5, 1e-9);
    }

    DoProjectTempoChange(track, 94.0);
    for (size_t i = 0; i < clips.size(); ++i) {
        EXPECT_DOUBLE_EQ(clips[i]->GetStretchRatio(), 2.0);
        EXPECT_NEAR(clips[i]->GetPlayStartTime(), 0.4 + i, 1e-9);
        EXPECT_NEAR(clips[i]->GetPlayEndTime(), 1.4 + i, 1e-9);
    }
}

TEST_F(ClipTempoPersistenceTests, LegacyStereoClipsKeepSpeedWhenChannelsAreJoined)
{
    ASSERT_TRUE(readProject({}, true));
    auto& track = *firstTrack();
    track.LinkConsistencyFix(true);
    EXPECT_EQ(track.NChannels(), 2);
    const auto clips = track.SortedIntervalArray();
    ASSERT_EQ(clips.size(), 2);
    for (const auto& clip : clips) {
        EXPECT_EQ(clip->NChannels(), 2);
        EXPECT_DOUBLE_EQ(clip->GetStretchRatio(), 1.0);
        EXPECT_NEAR(clip->GetPlayEndTime() - clip->GetPlayStartTime(), 0.5, 1e-9);
    }
}

TEST_F(ClipTempoPersistenceTests, ExplicitClipTempoIsPreservedWhenTempoMatchingIsDisabled)
{
    ASSERT_TRUE(readProject("clipTempo=\"150\" clipStretchToMatchTempo=\"0\""));
    auto& track = *firstTrack();
    track.LinkConsistencyFix(true);
    const auto clips = track.SortedIntervalArray();
    ASSERT_EQ(clips.size(), 2);
    for (const auto& clip : clips) {
        EXPECT_FALSE(clip->GetStretchToMatchProjectTempo());
        EXPECT_DOUBLE_EQ(clip->GetStretchRatio(), 188.0 / 150.0);
    }

    const auto trimLeft = clips.front()->GetTrimLeft();
    const auto trimRight = clips.front()->GetTrimRight();
    DoProjectTempoChange(track, 94.0);
    for (const auto& clip : clips) {
        EXPECT_DOUBLE_EQ(clip->GetStretchRatio(), 188.0 / 150.0);
        EXPECT_DOUBLE_EQ(clip->GetTrimLeft(), trimLeft);
        EXPECT_DOUBLE_EQ(clip->GetTrimRight(), trimRight);
    }
}

TEST_F(ClipTempoPersistenceTests, MissingClipTempoIsPreservedWhenTempoMatchingIsDisabled)
{
    ASSERT_TRUE(readProject("clipStretchToMatchTempo=\"0\""));
    const auto clips = firstTrack()->SortedIntervalArray();
    ASSERT_EQ(clips.size(), 2);
    for (const auto& clip : clips) {
        EXPECT_FALSE(clip->GetStretchToMatchProjectTempo());
        EXPECT_DOUBLE_EQ(clip->GetStretchRatio(), 188.0 / 130.0);
        EXPECT_DOUBLE_EQ(clip->GetEnvelope().GetTrackLen(), 188.0 / 130.0);
        XMLStringWriter writer;
        clip->WriteXML(0, writer);
        EXPECT_FALSE(writer.Contains("clipTempo="));
    }
}

TEST_F(ClipTempoPersistenceTests, MissingRawTempoIsNotInferredFromProjectTempo)
{
    ASSERT_TRUE(readProject("clipTempo=\"150\" clipStretchToMatchTempo=\"0\"", false, {}));
    const auto clips = firstTrack()->SortedIntervalArray();
    ASSERT_EQ(clips.size(), 2);
    for (const auto& clip : clips) {
        EXPECT_DOUBLE_EQ(clip->GetStretchRatio(), 188.0 / 130.0);
        EXPECT_DOUBLE_EQ(clip->GetEnvelope().GetTrackLen(), 188.0 / 130.0);
        XMLStringWriter writer;
        clip->WriteXML(0, writer);
        EXPECT_FALSE(writer.Contains("rawAudioTempo="));
    }
}

TEST_F(ClipTempoPersistenceTests, LegacySlowedClipsKeepEnvelopePointsBeyondRawDuration)
{
    ASSERT_TRUE(readProject({}, false, "rawAudioTempo=\"282\"", 1.0,
                            "<envelope numpoints=\"2\">"
                            "<controlpoint t=\"0\" val=\"0.8\"/>"
                            "<controlpoint t=\"1.1\" val=\"0.2\"/>"
                            "</envelope>"));
    const auto clips = firstTrack()->SortedIntervalArray();
    ASSERT_EQ(clips.size(), 2);
    for (const auto& clip : clips) {
        const auto& envelope = clip->GetEnvelope();
        EXPECT_DOUBLE_EQ(clip->GetStretchRatio(), 1.5);
        EXPECT_DOUBLE_EQ(envelope.GetTrackLen(), 1.5);
        ASSERT_EQ(envelope.GetNumberOfPoints(), 2);
        double times[2], values[2];
        envelope.GetPoints(times, values, 2);
        EXPECT_DOUBLE_EQ(times[0] + envelope.GetOffset(), 0.0);
        EXPECT_DOUBLE_EQ(values[0], 0.8);
        EXPECT_DOUBLE_EQ(times[1] + envelope.GetOffset(), 1.1);
        EXPECT_DOUBLE_EQ(values[1], 0.2);
    }
}

TEST_F(ClipTempoPersistenceTests, InheritedTempoSurvivesCopiesAndPaste)
{
    ASSERT_TRUE(readProject());
    const auto clip = firstTrack()->SortedIntervalArray().front();
    const auto& factory = WaveTrackFactory::Get(project()).GetSampleBlockFactory();
    auto copy = WaveClip::NewSharedFrom(*clip, factory, true, true);
    auto range = WaveClip::NewSharedFromRange(*clip, factory, false, true, 0.3, 0.6);
    auto pasted = WaveClip::NewShared(1, factory, floatSample, 48000);
    ASSERT_TRUE(pasted->Paste(0.0, *clip));
    for (const auto& result : { copy, range, pasted }) {
        EXPECT_DOUBLE_EQ(result->GetStretchRatio(), 1.0);
        XMLStringWriter writer;
        result->WriteXML(0, writer);
        EXPECT_FALSE(writer.Contains("clipTempo="));
    }
    EXPECT_NEAR(range->GetPlayStartTime(), 0.3, 1.0 / 48000.0);
    EXPECT_NEAR(range->GetPlayEndTime(), 0.6, 1.0 / 48000.0);
    EXPECT_NEAR(pasted->GetPlayEndTime(), 0.5, 1.0 / 48000.0);
}

TEST_F(ClipTempoPersistenceTests, PasteIntoMatchingClipPreservesSpeedOfUnmatchedSource)
{
    ASSERT_TRUE(readProject("clipStretchToMatchTempo=\"0\"", false, "rawAudioTempo=\"130\"", 1.0));
    const auto clip = firstTrack()->SortedIntervalArray().front();
    auto pasted = WaveClip::NewShared(1, WaveTrackFactory::Get(project()).GetSampleBlockFactory(), floatSample, 48000);
    pasted->OnProjectTempoChange(std::nullopt, 188.0);
    ASSERT_TRUE(pasted->Paste(0.0, *clip));
    EXPECT_TRUE(pasted->GetStretchToMatchProjectTempo());
    EXPECT_DOUBLE_EQ(pasted->GetStretchRatio(), 1.0);
}

TEST_F(ClipTempoPersistenceTests, PasteIntoUnmatchedClipPreservesInheritedSourceSpeed)
{
    ASSERT_TRUE(readProject());
    const auto clip = firstTrack()->SortedIntervalArray().front();
    auto pasted = WaveClip::NewShared(1, WaveTrackFactory::Get(project()).GetSampleBlockFactory(), floatSample, 48000);
    pasted->SetStretchToMatchProjectTempo(false);
    ASSERT_TRUE(pasted->Paste(0.0, *clip));
    EXPECT_FALSE(pasted->GetStretchToMatchProjectTempo());
    EXPECT_DOUBLE_EQ(pasted->GetStretchRatio(), 1.0);
}

TEST_F(ClipTempoPersistenceTests, DisablingMatchingFreezesInheritedTempo)
{
    ASSERT_TRUE(readProject());
    auto& track = *firstTrack();
    const auto clip = track.SortedIntervalArray().front();
    const auto duration = clip->GetPlayEndTime() - clip->GetPlayStartTime();
    clip->SetStretchToMatchProjectTempo(false);
    EXPECT_DOUBLE_EQ(clip->GetStretchRatio(), 1.0);
    EXPECT_DOUBLE_EQ(clip->GetPlayEndTime() - clip->GetPlayStartTime(), duration);

    DoProjectTempoChange(track, 94.0);
    EXPECT_DOUBLE_EQ(clip->GetStretchRatio(), 1.0);
    EXPECT_DOUBLE_EQ(clip->GetPlayEndTime() - clip->GetPlayStartTime(), duration);
    XMLStringWriter writer;
    clip->WriteXML(0, writer);
    EXPECT_TRUE(writer.Contains("clipTempo=\"188"));
}

TEST_F(ClipTempoPersistenceTests, EnablingMatchingPreservesSpeedAfterProjectTempoChanged)
{
    ASSERT_TRUE(readProject("clipStretchToMatchTempo=\"0\"", false, "rawAudioTempo=\"130\"", 1.0));
    auto& track = *firstTrack();
    const auto clip = track.SortedIntervalArray().front();
    DoProjectTempoChange(track, 94.0);
    EXPECT_DOUBLE_EQ(clip->GetStretchRatio(), 1.0);
    const auto start = clip->GetPlayStartTime();
    const auto end = clip->GetPlayEndTime();
    clip->SetStretchToMatchProjectTempo(true);
    EXPECT_DOUBLE_EQ(clip->GetStretchRatio(), 1.0);
    EXPECT_DOUBLE_EQ(clip->GetPlayStartTime(), start);
    EXPECT_DOUBLE_EQ(clip->GetPlayEndTime(), end);
}

TEST_F(ClipTempoPersistenceTests, ProjectTempoChangesUpdateExplicitTempoWhenMatchingIsEnabled)
{
    ASSERT_TRUE(readProject("clipTempo=\"150\""));
    auto& track = *firstTrack();
    const auto clip = track.SortedIntervalArray().front();
    EXPECT_DOUBLE_EQ(clip->GetStretchRatio(), 188.0 / 150.0);
    DoProjectTempoChange(track, 94.0);
    EXPECT_DOUBLE_EQ(clip->GetStretchRatio(), 2.0);
}

TEST_F(ClipTempoPersistenceTests, SaveAndReloadPreservesInheritedAndFrozenTempo)
{
    ASSERT_TRUE(readProject());
    firstTrack()->LinkConsistencyFix(true);
    {
        const auto clips = firstTrack()->SortedIntervalArray();
        clips.back()->SetStretchToMatchProjectTempo(false);
    }
    const auto path = m_directory.filePath("tempo.aup4").toStdString();
    ASSERT_TRUE(m_accessor.save(path));
    m_accessor.close();
    ASSERT_TRUE(m_accessor.load(path, false));

    const auto clips = firstTrack()->SortedIntervalArray();
    ASSERT_EQ(clips.size(), 2);
    EXPECT_TRUE(clips.front()->GetStretchToMatchProjectTempo());
    EXPECT_FALSE(clips.back()->GetStretchToMatchProjectTempo());
    for (const auto& clip : clips) {
        EXPECT_NEAR(clip->GetStretchRatio(), 1.0, 1e-8);
        EXPECT_NEAR(clip->GetPlayEndTime() - clip->GetPlayStartTime(), 0.5, 1.0 / 48000.0);
        XMLStringWriter writer;
        clip->WriteXML(0, writer);
        EXPECT_EQ(writer.Contains("clipTempo="), !clip->GetStretchToMatchProjectTempo());
    }
}

struct ClipTempoCase {
    const char* name;
    const char* attributes;
    const char* rawTempo;
    double expectedRatio;
};

class ClipTempoDefaultsTests : public ClipTempoPersistenceTests, public ::testing::WithParamInterface<ClipTempoCase>
{
};

TEST_P(ClipTempoDefaultsTests, PlaybackAndEnvelopeUseOnlyApplicableTempo)
{
    const auto& params = GetParam();
    ASSERT_TRUE(readProject(params.attributes, false, params.rawTempo, 1.0));
    const auto clips = firstTrack()->SortedIntervalArray();
    ASSERT_EQ(clips.size(), 2);
    for (size_t i = 0; i < clips.size(); ++i) {
        EXPECT_DOUBLE_EQ(clips[i]->GetStretchRatio(), params.expectedRatio);
        EXPECT_DOUBLE_EQ(clips[i]->GetEnvelope().GetTrackLen(), params.expectedRatio);
        EXPECT_NEAR(clips[i]->GetPlayStartTime(), 0.2 + 0.5 * i, 1e-9);
        // Playback boundaries are rounded to the sample grid.
        EXPECT_NEAR(clips[i]->GetPlayEndTime(), params.expectedRatio - 0.3 + 0.5 * i, 1.0 / 48000.0);
    }
}

TEST_P(ClipTempoDefaultsTests, TogglingMatchingPreservesPlaybackAndEnvelope)
{
    const auto& params = GetParam();
    ASSERT_TRUE(readProject(params.attributes, false, params.rawTempo, 1.0));
    for (const auto& clip : firstTrack()->SortedIntervalArray()) {
        const auto matching = clip->GetStretchToMatchProjectTempo();
        const auto start = clip->GetPlayStartTime();
        const auto end = clip->GetPlayEndTime();
        const auto envelopeLength = clip->GetEnvelope().GetTrackLen();
        for (bool enabled : { !matching, matching }) {
            clip->SetStretchToMatchProjectTempo(enabled);
            EXPECT_DOUBLE_EQ(clip->GetStretchRatio(), params.expectedRatio);
            EXPECT_DOUBLE_EQ(clip->GetPlayStartTime(), start);
            EXPECT_DOUBLE_EQ(clip->GetPlayEndTime(), end);
            EXPECT_DOUBLE_EQ(clip->GetEnvelope().GetTrackLen(), envelopeLength);
        }
    }
}

INSTANTIATE_TEST_SUITE_P(SerializedTempo, ClipTempoDefaultsTests, ::testing::Values(
                             ClipTempoCase { "NoTempoMetadata", "", "", 1.0 },
                             ClipTempoCase { "UnknownRawTempo", "", "rawAudioTempo=\"0\"", 1.0 },
                             ClipTempoCase { "RawTempoMatchesProject", "", "rawAudioTempo=\"188\"", 1.0 },
                             ClipTempoCase { "FollowProject", "clipStretchToMatchTempo=\"1\"", "rawAudioTempo=\"130\"", 130.0 / 188.0 },
                             ClipTempoCase { "DoNotFollowProject", "clipStretchToMatchTempo=\"0\"", "rawAudioTempo=\"130\"", 1.0 },
                             ClipTempoCase { "ExplicitTempo", "clipTempo=\"150\" clipStretchToMatchTempo=\"1\"", "rawAudioTempo=\"130\"",
                                             130.0 / 150.0 },
                             ClipTempoCase { "ExplicitTempoWithoutRawTempo", "clipTempo=\"150\"", "", 1.0 },
                             ClipTempoCase { "ExplicitTempoAfterDisabledMatching", "clipStretchToMatchTempo=\"0\" clipTempo=\"150\"",
                                             "rawAudioTempo=\"130\"", 130.0 / 150.0 }
                             ), [](const ::testing::TestParamInfo<ClipTempoCase>& info) { return info.param.name; });
}
