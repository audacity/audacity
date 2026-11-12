/*
 * Audacity: A Digital Audio Editor
 */
#include <gtest/gtest.h>

#include "projectrecovery/internal/binaryxml.h"

using namespace au::projectrecovery;

namespace {
struct TestBytes {
    std::vector<uint8_t> dict;
    std::vector<uint8_t> doc;
};

muse::RetVal<XmlDocument> decode(const TestBytes& bytes)
{
    return decodeBinaryXml({ muse::ByteArray(bytes.dict.data(), bytes.dict.size()), muse::ByteArray(bytes.doc.data(), bytes.doc.size()) });
}

void appendName(std::vector<uint8_t>& dict, uint16_t id, const std::string& name)
{
    dict.push_back(static_cast<uint8_t>(FieldType::Name));
    dict.push_back(static_cast<uint8_t>(id));
    dict.push_back(static_cast<uint8_t>(id >> 8));
    dict.push_back(static_cast<uint8_t>(name.size()));
    dict.push_back(static_cast<uint8_t>(name.size() >> 8));
    dict.insert(dict.end(), name.begin(), name.end());
}

void appendToken(std::vector<uint8_t>& doc, FieldType type, uint16_t id)
{
    doc.push_back(static_cast<uint8_t>(type));
    doc.push_back(static_cast<uint8_t>(id));
    doc.push_back(static_cast<uint8_t>(id >> 8));
}

void appendBytes(std::vector<uint8_t>& doc, std::initializer_list<uint8_t> bytes)
{
    doc.insert(doc.end(), bytes.begin(), bytes.end());
}
}

TEST(BinaryXmlTests, DecodesTheTokensOfProjectSerializer)
{
    //! [GIVEN] A dict and a doc built byte by byte as ProjectSerializer writes them, with one-byte characters
    TestBytes binary;
    binary.dict = { static_cast<uint8_t>(FieldType::CharSize), 1 };
    appendName(binary.dict, 0, "project");
    appendName(binary.dict, 1, "name");
    appendName(binary.dict, 2, "blockid");
    appendName(binary.dict, 3, "rate");
    appendName(binary.dict, 4, "selected");

    appendToken(binary.doc, FieldType::StartTag, 0);
    appendToken(binary.doc, FieldType::String, 1);
    appendBytes(binary.doc, { 3, 0, 0, 0, 'a', 'b', 'c' });
    appendToken(binary.doc, FieldType::LongLong, 2);
    appendBytes(binary.doc, { 0xFE, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF });
    appendToken(binary.doc, FieldType::Double, 3);
    const double rate = 48000;
    const auto* rateBytes = reinterpret_cast<const uint8_t*>(&rate);
    binary.doc.insert(binary.doc.end(), rateBytes, rateBytes + sizeof(rate));
    appendBytes(binary.doc, { 19, 0, 0, 0 });
    appendToken(binary.doc, FieldType::Bool, 4);
    appendBytes(binary.doc, { 1 });
    appendToken(binary.doc, FieldType::EndTag, 0);

    //! [WHEN] The document is decoded
    const muse::RetVal<XmlDocument> decoded = decode(binary);

    //! [THEN] Every attribute has its type and value
    ASSERT_TRUE(decoded.ret) << decoded.ret.toString();
    const XmlDocument& document = decoded.val;
    EXPECT_EQ(document.charSize, 1);
    ASSERT_EQ(document.elements.size(), 1u);
    const XmlNode& project = document.elements.front();
    EXPECT_EQ(project.tag, "project");
    ASSERT_EQ(project.attributes.size(), 4u);
    EXPECT_EQ(project.attribute("name")->text, "abc");
    EXPECT_EQ(project.attribute("blockid")->type, FieldType::LongLong);
    EXPECT_EQ(project.attribute("blockid")->integer, -2);
    // Start tag, then the string token, its length and "abc", then the type and name id of blockid
    EXPECT_EQ(project.attribute("blockid")->offset, binary.dict.size() + 3 + 3 + 4 + 3 + 3);
    EXPECT_EQ(project.attribute("rate")->real, 48000);
    EXPECT_EQ(project.attribute("rate")->digits, 19);
    EXPECT_EQ(project.attribute("selected")->toBool(), true);
}

TEST(BinaryXmlTests, DecodesTwoByteCharactersWithSurrogatePairs)
{
    //! [GIVEN] A document written on Windows: UTF-16 names and values, one value outside the basic plane
    TestBytes binary;
    binary.dict = { static_cast<uint8_t>(FieldType::CharSize), 2 };
    binary.dict.push_back(static_cast<uint8_t>(FieldType::Name));
    appendBytes(binary.dict, { 0, 0, 4, 0, 't', 0, 'g', 0 });
    binary.dict.push_back(static_cast<uint8_t>(FieldType::Name));
    appendBytes(binary.dict, { 1, 0, 2, 0, 'n', 0 });

    appendToken(binary.doc, FieldType::StartTag, 0);
    appendToken(binary.doc, FieldType::String, 1);
    // "Ж" (U+0416) and U+1F3B5 as a surrogate pair
    appendBytes(binary.doc, { 6, 0, 0, 0, 0x16, 0x04, 0x3C, 0xD8, 0xB5, 0xDF });
    appendToken(binary.doc, FieldType::EndTag, 0);

    //! [WHEN] The document is decoded
    const muse::RetVal<XmlDocument> decoded = decode(binary);

    //! [THEN] The names and the value are UTF-8
    ASSERT_TRUE(decoded.ret) << decoded.ret.toString();
    const XmlNode& node = decoded.val.elements.front();
    EXPECT_EQ(node.tag, "tg");
    EXPECT_EQ(node.attribute("n")->text, "\xD0\x96\xF0\x9F\x8E\xB5");
}

TEST(BinaryXmlTests, PopRestoresTheNamesOfTheOuterScope)
{
    //! [GIVEN] A Push that hides name 0, a new name 0 inside, and a Pop
    TestBytes binary;
    binary.dict = { static_cast<uint8_t>(FieldType::CharSize), 1 };
    appendName(binary.dict, 0, "outer");
    appendToken(binary.doc, FieldType::StartTag, 0);
    binary.doc.push_back(static_cast<uint8_t>(FieldType::Push));
    appendName(binary.doc, 0, "inner");
    appendToken(binary.doc, FieldType::StartTag, 0);
    appendToken(binary.doc, FieldType::EndTag, 0);
    binary.doc.push_back(static_cast<uint8_t>(FieldType::Pop));
    appendToken(binary.doc, FieldType::EndTag, 0);

    //! [WHEN] The document is decoded
    const muse::RetVal<XmlDocument> decoded = decode(binary);

    //! [THEN] The inner element has the inner name and the outer element closes with the outer name
    ASSERT_TRUE(decoded.ret) << decoded.ret.toString();
    const XmlNode& outer = decoded.val.elements.front();
    EXPECT_EQ(outer.tag, "outer");
    ASSERT_EQ(outer.children.size(), 1u);
    EXPECT_EQ(outer.children.front().tag, "inner");
}

TEST(BinaryXmlTests, RefusesElementsNestedTooDeep)
{
    const auto nested = [](int depth) {
        TestBytes bytes;
        bytes.dict = { static_cast<uint8_t>(FieldType::CharSize), 1 };
        appendName(bytes.dict, 0, "clip");
        for (int i = 0; i < depth; ++i) {
            appendToken(bytes.doc, FieldType::StartTag, 0);
        }
        for (int i = 0; i < depth; ++i) {
            appendToken(bytes.doc, FieldType::EndTag, 0);
        }
        return bytes;
    };

    EXPECT_TRUE(decode(nested(64)).ret);
    EXPECT_FALSE(decode(nested(65)).ret);
}

TEST(BinaryXmlTests, ReportsDamage)
{
    TestBytes valid;
    valid.dict = { static_cast<uint8_t>(FieldType::CharSize), 1 };
    appendName(valid.dict, 0, "project");
    appendName(valid.dict, 1, "rate");
    appendToken(valid.doc, FieldType::StartTag, 0);
    appendToken(valid.doc, FieldType::Int, 1);
    appendBytes(valid.doc, { 1, 0, 0, 0 });
    appendToken(valid.doc, FieldType::EndTag, 0);
    ASSERT_TRUE(decode(valid).ret);

    //! [THEN] A cut-off document, an unknown name, an unknown field type and an unclosed element are errors
    TestBytes cut = valid;
    cut.doc.resize(cut.doc.size() - 5);
    EXPECT_FALSE(decode(cut).ret);

    TestBytes unknownName = valid;
    unknownName.doc[1] = 7;
    EXPECT_FALSE(decode(unknownName).ret);

    TestBytes unknownType = valid;
    unknownType.doc[3] = 40;
    EXPECT_FALSE(decode(unknownType).ret);

    TestBytes unclosed = valid;
    unclosed.doc.resize(unclosed.doc.size() - 3);
    EXPECT_FALSE(decode(unclosed).ret);
}

TEST(BinaryXmlTests, EncodedDocumentDecodesToTheSameTree)
{
    for (const uint8_t charSize : { uint8_t(1), uint8_t(2), uint8_t(4) }) {
        //! [GIVEN] A document with every value type and texts, in each character size
        XmlDocument document;
        document.charSize = charSize;
        document.texts.push_back({ FieldType::Raw, "<?xml version=\"1.0\"?>" });
        XmlNode root;
        root.tag = "project";
        auto add = [&root](const std::string& name, FieldType type, int64_t integer, double real, const std::string& text) {
            XmlAttribute attribute;
            attribute.name = name;
            attribute.value.type = type;
            attribute.value.integer = integer;
            attribute.value.real = real;
            attribute.value.digits = type == FieldType::Float || type == FieldType::Double ? 7 : 0;
            attribute.value.text = text;
            root.attributes.push_back(attribute);
        };
        add("s", FieldType::String, 0, 0, "Тест \xF0\x9F\x8E\xB5");
        add("i", FieldType::Int, -5, 0, {});
        add("b", FieldType::Bool, 1, 0, {});
        add("l", FieldType::Long, 70000, 0, {});
        add("ll", FieldType::LongLong, -262144, 0, {});
        add("z", FieldType::SizeT, 262144, 0, {});
        add("f", FieldType::Float, 0, 0.5, {});
        add("d", FieldType::Double, 0, 44100.25, {});
        XmlAttribute blob;
        blob.name = "blob";
        blob.value.type = FieldType::Blob;
        const uint8_t blobBytes[] = { 0, 1, 2, 255 };
        blob.value.blob = muse::ByteArray(blobBytes, sizeof(blobBytes));
        root.attributes.push_back(blob);
        root.texts.push_back({ FieldType::Data, "data" });
        XmlNode child;
        child.tag = "wavetrack";
        root.children.push_back(child);
        document.elements.push_back(root);

        //! [WHEN] It is encoded and decoded
        const muse::RetVal<XmlDocument> decoded = decodeBinaryXml(encodeBinaryXml(document));

        //! [THEN] The tree is the same
        ASSERT_TRUE(decoded.ret) << decoded.ret.toString();
        const XmlDocument& result = decoded.val;
        EXPECT_EQ(result.charSize, charSize);
        ASSERT_EQ(result.texts.size(), 1u);
        EXPECT_EQ(result.texts.front().text, "<?xml version=\"1.0\"?>");
        ASSERT_EQ(result.elements.size(), 1u);
        const XmlNode& node = result.elements.front();
        ASSERT_EQ(node.attributes.size(), root.attributes.size());
        for (size_t i = 0; i < root.attributes.size(); ++i) {
            const XmlValue& expected = root.attributes[i].value;
            const XmlValue& actual = node.attributes[i].value;
            EXPECT_EQ(node.attributes[i].name, root.attributes[i].name);
            EXPECT_EQ(actual.type, expected.type);
            EXPECT_EQ(actual.integer, expected.integer);
            EXPECT_EQ(actual.real, expected.real);
            EXPECT_EQ(actual.digits, expected.digits);
            EXPECT_EQ(actual.text, expected.text);
            EXPECT_EQ(actual.blob, expected.blob);
        }
        ASSERT_EQ(node.texts.size(), 1u);
        EXPECT_EQ(node.texts.front().text, "data");
        ASSERT_EQ(node.children.size(), 1u);
        EXPECT_EQ(node.children.front().tag, "wavetrack");
    }
}
