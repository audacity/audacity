/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#ifndef MU_GLOBAL_XMLSTREAMREADER_H
#define MU_GLOBAL_XMLSTREAMREADER_H

#include <vector>
#include <map>

#include "io/iodevice.h"
#include "types/bytearray.h"
#include "types/string.h"

#ifndef NO_QT_SUPPORT
#include <QByteArray>
#endif

namespace mu {
class XmlStreamReader
{
public:

    enum TokenType {
        NoToken = 0,
        Invalid,
        StartDocument,
        EndDocument,
        StartElement,
        EndElement,
        Characters,
        Comment,
        DTD,
        Unknown
    };

    enum Error {
        NoError,
        UnexpectedElementError,
        CustomError,
        NotWellFormedError,
        PrematureEndOfDocumentError
    };

    struct Attribute
    {
        AsciiStringView name;
        String value;
    };

    XmlStreamReader();
    explicit XmlStreamReader(io::IODevice* device);
    explicit XmlStreamReader(const ByteArray& data);
#ifndef NO_QT_SUPPORT
    explicit XmlStreamReader(const QByteArray& data);
#endif
    virtual ~XmlStreamReader();

    XmlStreamReader(const XmlStreamReader&) = delete;
    XmlStreamReader& operator=(const XmlStreamReader&) = delete;

    void setData(const ByteArray& data);

    bool readNextStartElement();
    bool atEnd() const;
    void skipCurrentElement();
    TokenType readNext();
    TokenType tokenType() const;
    AsciiStringView tokenString() const;

    inline bool isStartDocument() const { return tokenType() == StartDocument; }
    inline bool isEndDocument() const { return tokenType() == EndDocument; }
    inline bool isStartElement() const { return tokenType() == StartElement; }
    inline bool isEndElement() const { return tokenType() == EndElement; }
    inline bool isCharacters() const { return tokenType() == Characters; }
    bool isWhitespace() const;

    AsciiStringView name() const;

    bool hasAttribute(const char* name) const;
    String attribute(const char* name) const;
    String attribute(const char* name, const String& def) const;
    AsciiStringView asciiAttribute(const char* name) const;
    AsciiStringView asciiAttribute(const char* name, const AsciiStringView& def) const;
    int intAttribute(const char* name) const;
    int intAttribute(const char* name, int def) const;
    double doubleAttribute(const char* name) const;
    double doubleAttribute(const char* name, double def) const;
    std::vector<Attribute> attributes() const;

    String text() const;
    AsciiStringView asciiText() const;
    String readText();
    AsciiStringView readAsciiText();
    int readInt(bool* ok = nullptr, int base = 10);
    double readDouble(bool* ok = nullptr);

    int64_t lineNumber() const;
    int64_t columnNumber() const;
    Error error() const;
    bool isError() const;
    String errorString() const;
    void raiseError(const String& message);

private:
    struct Xml;

    void tryParseEntity(Xml* xml);
    String nodeValue(Xml* xml) const;

    Xml* m_xml = nullptr;
    TokenType m_token = TokenType::NoToken;

    std::map<String, String> m_entities;
};
}

#endif // MU_GLOBAL_XMLSTREAMREADER_H
