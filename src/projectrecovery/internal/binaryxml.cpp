/*
 * Audacity: A Digital Audio Editor
 */
#include "binaryxml.h"

#include <cmath>
#include <cstdlib>
#include <cstring>
#include <map>
#include <unordered_map>
#include <utility>

using namespace au::projectrecovery;

namespace {
// Real project documents are about 6 levels deep
constexpr size_t MAX_ELEMENT_DEPTH = 64;

void appendUtf8(std::string& out, uint32_t codePoint)
{
    if (codePoint > 0x10FFFF || (codePoint >= 0xD800 && codePoint <= 0xDFFF)) {
        codePoint = 0xFFFD;
    }
    if (codePoint < 0x80) {
        out += static_cast<char>(codePoint);
    } else if (codePoint < 0x800) {
        out += static_cast<char>(0xC0 | (codePoint >> 6));
        out += static_cast<char>(0x80 | (codePoint & 0x3F));
    } else if (codePoint < 0x10000) {
        out += static_cast<char>(0xE0 | (codePoint >> 12));
        out += static_cast<char>(0x80 | ((codePoint >> 6) & 0x3F));
        out += static_cast<char>(0x80 | (codePoint & 0x3F));
    } else {
        out += static_cast<char>(0xF0 | (codePoint >> 18));
        out += static_cast<char>(0x80 | ((codePoint >> 12) & 0x3F));
        out += static_cast<char>(0x80 | ((codePoint >> 6) & 0x3F));
        out += static_cast<char>(0x80 | (codePoint & 0x3F));
    }
}

uint32_t readLittleEndian(const uint8_t* bytes, size_t size)
{
    uint32_t value = 0;
    for (size_t i = 0; i < size; ++i) {
        value |= static_cast<uint32_t>(bytes[i]) << (8 * i);
    }
    return value;
}

std::string decodeText(const uint8_t* bytes, size_t length, uint8_t charSize)
{
    if (charSize == 1) {
        return std::string(reinterpret_cast<const char*>(bytes), length);
    }
    std::string text;
    if (charSize == 2) {
        for (size_t i = 0; i + 1 < length; i += 2) {
            uint32_t unit = readLittleEndian(bytes + i, 2);
            if (unit >= 0xD800 && unit <= 0xDBFF && i + 3 < length) {
                const uint32_t low = readLittleEndian(bytes + i + 2, 2);
                if (low >= 0xDC00 && low <= 0xDFFF) {
                    unit = 0x10000 + ((unit - 0xD800) << 10) + (low - 0xDC00);
                    i += 2;
                }
            }
            appendUtf8(text, unit);
        }
    } else {
        for (size_t i = 0; i + 3 < length; i += 4) {
            appendUtf8(text, readLittleEndian(bytes + i, 4));
        }
    }
    return text;
}

std::vector<uint32_t> codePoints(const std::string& text)
{
    std::vector<uint32_t> result;
    for (size_t i = 0; i < text.size();) {
        const uint8_t lead = static_cast<uint8_t>(text[i]);
        size_t count = 1;
        uint32_t codePoint = lead;
        if ((lead >> 5) == 0x6) {
            count = 2;
            codePoint = lead & 0x1F;
        } else if ((lead >> 4) == 0xE) {
            count = 3;
            codePoint = lead & 0x0F;
        } else if ((lead >> 3) == 0x1E) {
            count = 4;
            codePoint = lead & 0x07;
        } else if (lead >= 0x80) {
            codePoint = 0xFFFD;
        }
        if (i + count > text.size()) {
            result.push_back(0xFFFD);
            break;
        }
        for (size_t k = 1; k < count; ++k) {
            codePoint = (codePoint << 6) | (static_cast<uint8_t>(text[i + k]) & 0x3F);
        }
        result.push_back(codePoint);
        i += count;
    }
    return result;
}

void appendLittleEndian(std::vector<uint8_t>& out, uint64_t value, size_t size)
{
    for (size_t i = 0; i < size; ++i) {
        out.push_back(static_cast<uint8_t>(value >> (8 * i)));
    }
}

std::vector<uint8_t> encodeText(const std::string& text, uint8_t charSize)
{
    if (charSize == 1) {
        return std::vector<uint8_t>(text.begin(), text.end());
    }
    std::vector<uint8_t> out;
    for (const uint32_t codePoint : codePoints(text)) {
        if (charSize == 4) {
            appendLittleEndian(out, codePoint, 4);
        } else if (codePoint >= 0x10000) {
            const uint32_t value = codePoint - 0x10000;
            appendLittleEndian(out, 0xD800 + (value >> 10), 2);
            appendLittleEndian(out, 0xDC00 + (value & 0x3FF), 2);
        } else {
            appendLittleEndian(out, codePoint, 2);
        }
    }
    return out;
}

void appendValue(std::vector<uint8_t>& out, const XmlValue& value, uint8_t charSize)
{
    switch (value.type) {
    case FieldType::String: {
        const std::vector<uint8_t> text = encodeText(value.text, charSize);
        appendLittleEndian(out, text.size(), 4);
        out.insert(out.end(), text.begin(), text.end());
        break;
    }
    case FieldType::Blob:
        appendLittleEndian(out, value.blob.size(), 4);
        out.insert(out.end(), value.blob.constData(), value.blob.constData() + value.blob.size());
        break;
    case FieldType::Bool:
        out.push_back(value.integer != 0 ? 1 : 0);
        break;
    case FieldType::Int:
    case FieldType::Long:
    case FieldType::SizeT:
        appendLittleEndian(out, static_cast<uint64_t>(value.integer), 4);
        break;
    case FieldType::LongLong:
        appendLittleEndian(out, static_cast<uint64_t>(value.integer), 8);
        break;
    case FieldType::Float: {
        const float real = static_cast<float>(value.real);
        uint32_t bits = 0;
        std::memcpy(&bits, &real, sizeof(bits));
        appendLittleEndian(out, bits, 4);
        appendLittleEndian(out, static_cast<uint32_t>(value.digits), 4);
        break;
    }
    case FieldType::Double: {
        uint64_t bits = 0;
        std::memcpy(&bits, &value.real, sizeof(bits));
        appendLittleEndian(out, bits, 8);
        appendLittleEndian(out, static_cast<uint32_t>(value.digits), 4);
        break;
    }
    default:
        break;
    }
}

class ByteReader
{
public:
    explicit ByteReader(const std::vector<uint8_t>& bytes)
        : m_bytes(bytes) {}

    size_t position() const { return m_position; }
    bool atEnd() const { return m_position >= m_bytes.size(); }
    bool has(size_t count) const { return m_position + count <= m_bytes.size(); }
    const uint8_t* current() const { return m_bytes.data() + m_position; }
    void skip(size_t count) { m_position += count; }

    uint64_t read(size_t size)
    {
        uint64_t value = 0;
        for (size_t i = 0; i < size; ++i) {
            value |= static_cast<uint64_t>(m_bytes[m_position + i]) << (8 * i);
        }
        m_position += size;
        return value;
    }

private:
    const std::vector<uint8_t>& m_bytes;
    size_t m_position = 0;
};

bool isAttribute(FieldType type)
{
    switch (type) {
    case FieldType::String:
    case FieldType::Int:
    case FieldType::Bool:
    case FieldType::Long:
    case FieldType::LongLong:
    case FieldType::SizeT:
    case FieldType::Float:
    case FieldType::Double:
    case FieldType::Blob:
        return true;
    default:
        return false;
    }
}

//! The size of the value after the name id; 0 for a value with a length prefix
size_t fixedValueSize(FieldType type)
{
    switch (type) {
    case FieldType::Bool:
        return 1;
    case FieldType::Int:
    case FieldType::Long:
    case FieldType::SizeT:
        return 4;
    case FieldType::LongLong:
    case FieldType::Float:
        return 8;
    case FieldType::Double:
        return 12;
    default:
        return 0;
    }
}

XmlValue readFixedValue(ByteReader& in, FieldType type)
{
    XmlValue value;
    value.type = type;
    switch (type) {
    case FieldType::Bool:
        value.integer = static_cast<int64_t>(in.read(1));
        break;
    case FieldType::Int:
    case FieldType::Long:
        value.integer = static_cast<int32_t>(in.read(4));
        break;
    case FieldType::SizeT:
        value.integer = static_cast<int64_t>(in.read(4));
        break;
    case FieldType::LongLong:
        value.integer = static_cast<int64_t>(in.read(8));
        break;
    case FieldType::Float: {
        const uint32_t bits = static_cast<uint32_t>(in.read(4));
        float real = 0;
        std::memcpy(&real, &bits, sizeof(real));
        value.real = real;
        value.digits = static_cast<int32_t>(in.read(4));
        break;
    }
    case FieldType::Double: {
        const uint64_t bits = in.read(8);
        std::memcpy(&value.real, &bits, sizeof(value.real));
        value.digits = static_cast<int32_t>(in.read(4));
        break;
    }
    default:
        break;
    }
    return value;
}
}

std::optional<int64_t> XmlValue::toInteger() const
{
    switch (type) {
    case FieldType::Int:
    case FieldType::Bool:
    case FieldType::Long:
    case FieldType::LongLong:
    case FieldType::SizeT:
        return integer;
    case FieldType::Float:
    case FieldType::Double:
        return std::llround(real);
    case FieldType::String: {
        char* end = nullptr;
        const long long parsed = std::strtoll(text.c_str(), &end, 10);
        if (end != text.c_str() && *end == '\0') {
            return parsed;
        }
        return std::nullopt;
    }
    default:
        return std::nullopt;
    }
}

std::optional<double> XmlValue::toDouble() const
{
    switch (type) {
    case FieldType::Float:
    case FieldType::Double:
        return real;
    case FieldType::Int:
    case FieldType::Bool:
    case FieldType::Long:
    case FieldType::LongLong:
    case FieldType::SizeT:
        return static_cast<double>(integer);
    case FieldType::String: {
        char* end = nullptr;
        const double parsed = std::strtod(text.c_str(), &end);
        if (end != text.c_str() && *end == '\0') {
            return parsed;
        }
        return std::nullopt;
    }
    default:
        return std::nullopt;
    }
}

std::optional<bool> XmlValue::toBool() const
{
    if (type == FieldType::String && (text == "true" || text == "false")) {
        return text == "true";
    }
    const auto number = toInteger();
    if (!number) {
        return std::nullopt;
    }
    return *number != 0;
}

const XmlValue* XmlNode::attribute(std::string_view name) const
{
    for (const XmlAttribute& attribute : attributes) {
        if (attribute.name == name) {
            return &attribute.value;
        }
    }
    return nullptr;
}

muse::RetVal<XmlDocument> au::projectrecovery::decodeBinaryXml(const BinaryXml& binary)
{
    // The decoder reads the dict and the doc as one stream, as ProjectSerializer::Decode does
    std::vector<uint8_t> bytes;
    bytes.reserve(binary.dict.size() + binary.doc.size());
    bytes.insert(bytes.end(), binary.dict.constData(), binary.dict.constData() + binary.dict.size());
    bytes.insert(bytes.end(), binary.doc.constData(), binary.doc.constData() + binary.doc.size());

    XmlDocument document;
    uint8_t charSize = 0;
    std::unordered_map<uint16_t, std::string> names;
    std::vector<std::unordered_map<uint16_t, std::string> > savedNames;
    XmlNode top;
    std::vector<XmlNode*> open { &top };

    ByteReader in(bytes);
    auto fail = [&in](const std::string& what) {
        return muse::RetVal<XmlDocument>::make_ret(muse::make_ret(muse::Ret::Code::BadData,
                                                                  "the document is damaged: " + what + " at byte "
                                                                  + std::to_string(in.position())));
    };
    auto readName = [&](std::string& name) {
        if (!in.has(2)) {
            return false;
        }
        const auto found = names.find(static_cast<uint16_t>(in.read(2)));
        if (found == names.end()) {
            return false;
        }
        name = found->second;
        return true;
    };

    while (!in.atEnd()) {
        const auto type = static_cast<FieldType>(in.read(1));
        switch (type) {
        case FieldType::CharSize:
            if (!in.has(1)) {
                return fail("the character size is cut off");
            }
            charSize = static_cast<uint8_t>(in.read(1));
            if (charSize != 1 && charSize != 2 && charSize != 4) {
                return fail("the character size is not 1, 2 or 4");
            }
            document.charSize = charSize;
            break;
        case FieldType::Push:
            savedNames.push_back(std::move(names));
            names.clear();
            break;
        case FieldType::Pop:
            if (savedNames.empty()) {
                return fail("a name scope ends that did not start");
            }
            names = std::move(savedNames.back());
            savedNames.pop_back();
            break;
        case FieldType::Name: {
            if (!in.has(4) || charSize == 0) {
                return fail("a name is cut off or comes before the character size");
            }
            const auto id = static_cast<uint16_t>(in.read(2));
            const auto length = static_cast<size_t>(in.read(2));
            if (!in.has(length)) {
                return fail("a name is cut off");
            }
            names[id] = decodeText(in.current(), length, charSize);
            in.skip(length);
            break;
        }
        case FieldType::StartTag: {
            XmlNode node;
            if (!readName(node.tag)) {
                return fail("a tag has an unknown name");
            }
            if (open.size() > MAX_ELEMENT_DEPTH) {
                return fail("the elements are nested too deep");
            }
            open.back()->children.push_back(std::move(node));
            open.push_back(&open.back()->children.back());
            break;
        }
        case FieldType::EndTag: {
            std::string tag;
            if (!readName(tag)) {
                return fail("an end tag has an unknown name");
            }
            if (open.size() < 2 || open.back()->tag != tag) {
                return fail("the end tag \"" + tag + "\" does not close the open element");
            }
            open.pop_back();
            break;
        }
        case FieldType::Data:
        case FieldType::Raw: {
            if (!in.has(4) || charSize == 0) {
                return fail("a text is cut off or comes before the character size");
            }
            const auto length = static_cast<size_t>(in.read(4));
            if (!in.has(length)) {
                return fail("a text is cut off");
            }
            open.back()->texts.push_back({ type, decodeText(in.current(), length, charSize) });
            in.skip(length);
            break;
        }
        default: {
            if (!isAttribute(type)) {
                return fail("the field type " + std::to_string(static_cast<int>(type)) + " is unknown");
            }
            XmlAttribute attribute;
            if (!readName(attribute.name)) {
                return fail("an attribute has an unknown name");
            }
            if (open.size() < 2) {
                return fail("an attribute is outside of all elements");
            }
            if (type == FieldType::String || type == FieldType::Blob) {
                if (!in.has(4)) {
                    return fail("a value is cut off");
                }
                const auto length = static_cast<size_t>(in.read(4));
                if (!in.has(length) || (type == FieldType::String && charSize == 0)) {
                    return fail("a value is cut off or comes before the character size");
                }
                attribute.value.type = type;
                if (type == FieldType::String) {
                    attribute.value.text = decodeText(in.current(), length, charSize);
                } else {
                    attribute.value.blob = muse::ByteArray(in.current(), length);
                }
                in.skip(length);
            } else {
                if (!in.has(fixedValueSize(type))) {
                    return fail("a value is cut off");
                }
                attribute.value = readFixedValue(in, type);
            }
            open.back()->attributes.push_back(std::move(attribute));
            break;
        }
        }
    }

    if (open.size() != 1) {
        return fail("the element \"" + open.back()->tag + "\" is not closed");
    }
    document.texts = std::move(top.texts);
    document.elements = std::move(top.children);
    return muse::RetVal<XmlDocument>::make_ok(document);
}

BinaryXml au::projectrecovery::encodeBinaryXml(const XmlDocument& document)
{
    std::vector<uint8_t> dict;
    std::vector<uint8_t> doc;
    const uint8_t charSize = document.charSize;
    dict.push_back(static_cast<uint8_t>(FieldType::CharSize));
    dict.push_back(charSize);

    std::map<std::string, uint16_t> ids;
    auto nameId = [&](const std::string& name) {
        const auto found = ids.find(name);
        if (found != ids.end()) {
            return found->second;
        }
        const auto id = static_cast<uint16_t>(ids.size());
        ids.emplace(name, id);
        const std::vector<uint8_t> text = encodeText(name, charSize);
        dict.push_back(static_cast<uint8_t>(FieldType::Name));
        appendLittleEndian(dict, id, 2);
        appendLittleEndian(dict, text.size(), 2);
        dict.insert(dict.end(), text.begin(), text.end());
        return id;
    };

    auto writeText = [&](const XmlText& text) {
        const std::vector<uint8_t> bytes = encodeText(text.text, charSize);
        doc.push_back(static_cast<uint8_t>(text.type));
        appendLittleEndian(doc, bytes.size(), 4);
        doc.insert(doc.end(), bytes.begin(), bytes.end());
    };
    auto writeNode = [&](const XmlNode& node, auto& self) -> void {
        const uint16_t tagId = nameId(node.tag);
        doc.push_back(static_cast<uint8_t>(FieldType::StartTag));
        appendLittleEndian(doc, tagId, 2);
        for (const XmlAttribute& attribute : node.attributes) {
            const uint16_t id = nameId(attribute.name);
            doc.push_back(static_cast<uint8_t>(attribute.value.type));
            appendLittleEndian(doc, id, 2);
            appendValue(doc, attribute.value, charSize);
        }
        for (const XmlText& text : node.texts) {
            writeText(text);
        }
        for (const XmlNode& child : node.children) {
            self(child, self);
        }
        doc.push_back(static_cast<uint8_t>(FieldType::EndTag));
        appendLittleEndian(doc, tagId, 2);
    };

    for (const XmlText& text : document.texts) {
        writeText(text);
    }
    for (const XmlNode& element : document.elements) {
        writeNode(element, writeNode);
    }
    return { muse::ByteArray(dict.data(), dict.size()), muse::ByteArray(doc.data(), doc.size()) };
}
