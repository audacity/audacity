/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "projectrecovery/internal/documentedit.h"
#include "projectrecovery/internal/projectdocument.h"

#include "testprojectbuilder.h"

using namespace au::projectrecovery;
using namespace au::projectrecovery::tests;

namespace {
ProjectDocument readSilenced(const BinaryXml& binary, const muse::ByteArray& doc)
{
    const muse::RetVal<XmlDocument> decoded = decodeBinaryXml({ binary.dict, doc });
    EXPECT_TRUE(decoded.ret) << decoded.ret.toString();
    return readProjectDocument(decoded.val).value_or(ProjectDocument());
}

XmlNode& firstClip(XmlDocument& document)
{
    return document.elements.front().children.front().children.front();
}
}

TEST(DocumentEditTests, SilencedBlocksHaveTheirOwnLengthAndNoOtherByteChanges)
{
    //! [GIVEN] A document with three blocks in two clips
    TestProject test;
    test.tracks.push_back({ "Voice", 44100, { { "Take 1", 1.0, { { 1, 262144 }, { 2, 262144 } } }, { "Take 2", 20.0, { { 3, 1000 } } } } });
    const BinaryXml binary = encodeBinaryXml(makeDocument(test));

    //! [WHEN] Blocks 1 and 3 are silenced
    const muse::RetVal<muse::ByteArray> doc = silenceBlocks(binary, { 1, 3 });

    //! [THEN] Their ids are minus their lengths, and block 2 keeps its id
    ASSERT_TRUE(doc.ret) << doc.ret.toString();
    const ProjectDocument project = readSilenced(binary, doc.val);
    ASSERT_EQ(project.waveTracks.size(), 1u);
    const auto& clips = project.waveTracks.front().clips;
    ASSERT_EQ(clips.size(), 2u);
    EXPECT_EQ(clips[0].sequences[0].blocks[0].blockId, -262144);
    EXPECT_EQ(clips[0].sequences[0].blocks[1].blockId, 2);
    EXPECT_EQ(clips[1].sequences[0].blocks[0].blockId, -1000);

    //! [THEN] Only the 8 bytes of each silenced id are different
    ASSERT_EQ(doc.val.size(), binary.doc.size());
    std::vector<bool> patched(binary.doc.size(), false);
    for (const WaveBlock* block : { &clips[0].sequences[0].blocks[0], &clips[1].sequences[0].blocks[0] }) {
        for (size_t i = 0; i < 8; ++i) {
            patched[block->blockIdOffset - binary.dict.size() + i] = true;
        }
    }
    for (size_t i = 0; i < binary.doc.size(); ++i) {
        if (!patched[i]) {
            EXPECT_EQ(doc.val.constData()[i], binary.doc.constData()[i]) << "byte " << i;
        }
    }
}

TEST(DocumentEditTests, EveryReferenceIsSilencedAlsoInCutLines)
{
    //! [GIVEN] A clip whose cut line references the same block
    TestProject test;
    test.tracks.push_back({ "Voice", 44100, { { "Take 1", 0, { { 5, 100 } } } } });
    XmlDocument document = makeDocument(test);
    const XmlNode cutLine = firstClip(document);
    firstClip(document).children.push_back(cutLine);
    const BinaryXml binary = encodeBinaryXml(document);

    //! [WHEN] The block is silenced
    const muse::RetVal<muse::ByteArray> doc = silenceBlocks(binary, { 5 });

    //! [THEN] Both references are silence
    ASSERT_TRUE(doc.ret) << doc.ret.toString();
    const ProjectDocument project = readSilenced(binary, doc.val);
    const WaveClip& clip = project.waveTracks.front().clips.front();
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
    XmlValue& intId = firstClip(intDocument).children.front().children.front().attributes[1].value;
    ASSERT_EQ(intId.integer, 5);
    intId.type = FieldType::Int;
    XmlDocument sizeDocument = intDocument;
    firstClip(sizeDocument).children.front().children.front().attributes[1].value.type = FieldType::SizeT;

    //! [THEN] Audacity reads a negative Int, so it is silenced; a SizeT cannot be negative, so it is an error
    const BinaryXml intBinary = encodeBinaryXml(intDocument);
    const muse::RetVal<muse::ByteArray> intDoc = silenceBlocks(intBinary, { 5 });
    ASSERT_TRUE(intDoc.ret) << intDoc.ret.toString();
    EXPECT_EQ(readSilenced(intBinary, intDoc.val).waveTracks.front().clips.front().sequences[0].blocks[0].blockId, -100);

    EXPECT_FALSE(silenceBlocks(encodeBinaryXml(sizeDocument), { 5 }).ret);
}
