/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "projectrecovery/internal/projectdocument.h"

#include "testprojectbuilder.h"

using namespace au::projectrecovery;
using namespace au::projectrecovery::tests;

namespace {
XmlAttribute doubleValue(const std::string& name, double value)
{
    XmlAttribute attribute;
    attribute.name = name;
    attribute.value.type = FieldType::Double;
    attribute.value.real = value;
    return attribute;
}
}

TEST(ProjectDocumentTests, BlockLengthsComeFromTheNextStartAndTheSampleCount)
{
    //! [GIVEN] A clip with a full block, a silent block and a short last block
    TestProject test;
    test.tracks.push_back({ "Voice", 48000, { { "Take 1", 2.5, { { 1, 262144 }, { -1000, 1000 }, { 2, 500 } } } } });

    //! [WHEN] The model is read
    const auto project = readProjectDocument(makeDocument(test));

    //! [THEN] Each block has its length, and the track keeps its own rate
    ASSERT_TRUE(project.has_value());
    ASSERT_EQ(project->waveTracks.size(), 1u);
    const WaveTrack& track = project->waveTracks.front();
    EXPECT_EQ(track.name, "Voice");
    EXPECT_EQ(track.rate, 48000);
    ASSERT_EQ(track.clips.size(), 1u);
    const WaveClip& clip = track.clips.front();
    EXPECT_EQ(clip.name, "Take 1");
    EXPECT_EQ(clip.offset, 2.5);
    ASSERT_EQ(clip.sequences.size(), 1u);
    const Sequence& sequence = clip.sequences.front();
    EXPECT_EQ(sequence.numSamples, 263644);
    ASSERT_EQ(sequence.blocks.size(), 3u);
    EXPECT_EQ(sequence.blocks[0].length, 262144);
    EXPECT_EQ(sequence.blocks[1].blockId, -1000);
    EXPECT_EQ(sequence.blocks[1].length, 1000);
    EXPECT_EQ(sequence.blocks[2].start, 263144);
    EXPECT_EQ(sequence.blocks[2].length, 500);
}

TEST(ProjectDocumentTests, DocumentWithoutProjectElementIsNotAProject)
{
    XmlDocument document;
    XmlNode other;
    other.tag = "audacityproject";
    document.elements.push_back(other);

    EXPECT_FALSE(readProjectDocument(document).has_value());
}

TEST(ProjectDocumentTests, LegacyTempoAttributesChangeTheStretch)
{
    //! [GIVEN] A project at 120 BPM
    ProjectDocument project;
    project.tempo = 120;

    //! [THEN] A clip without the legacy tempo keeps its own ratio
    WaveClip clip;
    clip.stretchRatio = 2;
    EXPECT_EQ(effectiveStretchRatio(clip, project), 2);

    //! [THEN] A clip recorded at 60 BPM that follows the project tempo plays at half the speed
    clip.stretchRatio = 1;
    clip.rawAudioTempo = 60;
    EXPECT_EQ(effectiveStretchRatio(clip, project), 0.5);

    //! [THEN] A clip tempo replaces the project tempo
    clip.clipTempo = 30;
    EXPECT_EQ(effectiveStretchRatio(clip, project), 2);

    //! [THEN] A clip that does not follow the project tempo and has no clip tempo keeps its ratio
    clip.clipTempo.reset();
    clip.stretchToMatchTempo = false;
    EXPECT_EQ(effectiveStretchRatio(clip, project), 1);
}

TEST(ProjectDocumentTests, TrackWithoutRateUsesTheProjectRate)
{
    XmlDocument document = makeDocument({ { { "A", 0, {} } } });
    XmlNode& track = document.elements.front().children.front();
    std::erase_if(track.attributes, [](const XmlAttribute& attribute) { return attribute.name == "rate"; });
    XmlNode& root = document.elements.front();
    std::erase_if(root.attributes, [](const XmlAttribute& attribute) { return attribute.name == "rate"; });
    root.attributes.push_back(doubleValue("rate", 22050));

    const auto project = readProjectDocument(document);

    ASSERT_TRUE(project.has_value());
    EXPECT_EQ(project->waveTracks.front().rate, 22050);
}
