/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "framework/global/types/bytearray.h"
#include "framework/global/types/retval.h"

namespace au::projectrecovery {
//! The field types of the binary project document, as ProjectSerializer writes them
enum class FieldType : uint8_t {
    CharSize = 0,
    StartTag,
    EndTag,
    String,
    Int,
    Bool,
    Long,
    LongLong,
    SizeT,
    Float,
    Double,
    Data,
    Raw,
    Push,
    Pop,
    Name,
    Blob,
};

struct XmlValue {
    FieldType type = FieldType::String;
    //! Int, Bool, Long, LongLong, SizeT
    int64_t integer = 0;
    //! Float, Double
    double real = 0;
    int32_t digits = 0;
    //! String, as UTF-8
    std::string text;
    muse::ByteArray blob;

    std::optional<int64_t> toInteger() const;
    std::optional<double> toDouble() const;
    std::optional<bool> toBool() const;
};

struct XmlAttribute {
    std::string name;
    XmlValue value;
};

//! Data or Raw text, as UTF-8
struct XmlText {
    FieldType type = FieldType::Data;
    std::string text;
};

struct XmlNode {
    std::string tag;
    std::vector<XmlAttribute> attributes;
    std::vector<XmlText> texts;
    std::vector<XmlNode> children;

    const XmlValue* attribute(std::string_view name) const;
};

struct XmlDocument {
    //! The size of one character in bytes: 1 (UTF-8), 2 (UTF-16) or 4 (UTF-32)
    uint8_t charSize = 1;
    //! Text outside of all elements
    std::vector<XmlText> texts;
    std::vector<XmlNode> elements;
};

//! The two blobs of a document row: the names, then the tokens that use them
struct BinaryXml {
    muse::ByteArray dict;
    muse::ByteArray doc;
};

muse::RetVal<XmlDocument> decodeBinaryXml(const BinaryXml& binary);

//! Gives a document that decodes to the same tree; the name ids can differ from the ones Audacity wrote
BinaryXml encodeBinaryXml(const XmlDocument& document);
}
