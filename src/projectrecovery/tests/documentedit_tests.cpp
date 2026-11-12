/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include <stdexcept>

#include "projectrecovery/internal/documentedit.h"
#include "projectrecovery/internal/projectdocument.h"

#include "testprojectbuilder.h"

using namespace au::projectrecovery;
using namespace au::projectrecovery::tests;

namespace {
XmlNode& clipNode(XmlDocument& document, size_t index)
{
    return document.elements.front().children.front().children[index];
}

XmlValue& blockIdValue(XmlDocument& document, size_t clip, size_t block)
{
    for (XmlAttribute& attribute : clipNode(document, clip).children.front().children[block].attributes) {
        if (attribute.name == "blockid") {
            return attribute.value;
        }
    }
    throw std::out_of_range("no blockid attribute");
}
}

TEST(DocumentEditTests, SilencedBlocksHaveTheirOwnLengthAndNothingElseChanges)
{
    //! [GIVEN] A document with three blocks in two clips
    TestProject test;
    test.tracks.push_back({ "Voice", 44100, { { "Take 1", 1.0, { { 1, 262144 }, { 2, 262144 } } }, { "Take 2", 20.0, { { 3, 1000 } } } } });
    XmlDocument document = makeDocument(test);

    //! [WHEN] Blocks 1 and 3 are silenced
    const muse::Ret ret = silenceBlocks(document, { 1, 3 });

    //! [THEN] Their ids are minus their lengths, and block 2 keeps its id
    ASSERT_TRUE(ret) << ret.toString();
    const std::optional<ProjectDocument> project = readProjectDocument(document);
    ASSERT_TRUE(project.has_value());
    const auto& clips = project->waveTracks.front().clips;
    ASSERT_EQ(clips.size(), 2u);
    EXPECT_EQ(clips[0].sequences[0].blocks[0].blockId, -262144);
    EXPECT_EQ(clips[0].sequences[0].blocks[1].blockId, 2);
    EXPECT_EQ(clips[1].sequences[0].blocks[0].blockId, -1000);

    //! [THEN] Nothing else in the document changes
    XmlDocument expected = makeDocument(test);
    blockIdValue(expected, 0, 0).integer = -262144;
    blockIdValue(expected, 1, 0).integer = -1000;
    const BinaryXml expectedBinary = encodeBinaryXml(expected);
    const BinaryXml silencedBinary = encodeBinaryXml(document);
    EXPECT_EQ(silencedBinary.dict, expectedBinary.dict);
    EXPECT_EQ(silencedBinary.doc, expectedBinary.doc);
}

TEST(DocumentEditTests, EveryReferenceIsSilencedAlsoInCutLines)
{
    //! [GIVEN] A clip whose cut line references the same block
    TestProject test;
    test.tracks.push_back({ "Voice", 44100, { { "Take 1", 0, { { 5, 100 } } } } });
    XmlDocument document = makeDocument(test);
    const XmlNode cutLine = clipNode(document, 0);
    clipNode(document, 0).children.push_back(cutLine);

    //! [WHEN] The block is silenced
    const muse::Ret ret = silenceBlocks(document, { 5 });

    //! [THEN] Both references are silence
    ASSERT_TRUE(ret) << ret.toString();
    const std::optional<ProjectDocument> project = readProjectDocument(document);
    ASSERT_TRUE(project.has_value());
    const WaveClip& clip = project->waveTracks.front().clips.front();
    EXPECT_EQ(clip.sequences[0].blocks[0].blockId, -100);
    ASSERT_EQ(clip.cutLines.size(), 1u);
    EXPECT_EQ(clip.cutLines[0].sequences[0].blocks[0].blockId, -100);
}

TEST(DocumentEditTests, BlockIdsOfOtherTypesAreNotChanged)
{
    //! [GIVEN] A block id written as a 4-byte Int, and one written as an unsigned SizeT
    TestProject test;
    test.tracks.push_back({ "Voice", 44100, { { "Take 1", 0, { { 5, 100 } } } } });
    XmlDocument intDocument = makeDocument(test);
    ASSERT_EQ(blockIdValue(intDocument, 0, 0).integer, 5);
    blockIdValue(intDocument, 0, 0).type = FieldType::Int;
    XmlDocument sizeDocument = intDocument;
    blockIdValue(sizeDocument, 0, 0).type = FieldType::SizeT;

    //! [THEN] Audacity reads a negative Int, so it is silenced; a SizeT cannot be negative, so it is an error
    const muse::Ret intRet = silenceBlocks(intDocument, { 5 });
    ASSERT_TRUE(intRet) << intRet.toString();
    EXPECT_EQ(blockIdValue(intDocument, 0, 0).integer, -100);

    EXPECT_FALSE(silenceBlocks(sizeDocument, { 5 }));
}
