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
#include "xmlstreamreader.h"

#include <cstring>

#include "global/types/string.h"

#include "../thirdparty/tinyxml/tinyxml2.h"

#include "log.h"

using namespace mu;
using namespace mu::io;
using namespace tinyxml2;

struct XmlStreamReader::Xml {
    XMLDocument doc;
    XMLNode* node = nullptr;
    XMLError err;
    String customErr;
};

XmlStreamReader::XmlStreamReader()
{
    m_xml = new Xml();
}

XmlStreamReader::XmlStreamReader(IODevice* device)
{
    m_xml = new Xml();
    ByteArray data = device->readAll();
    setData(data);
}

XmlStreamReader::XmlStreamReader(const ByteArray& data)
{
    m_xml = new Xml();
    setData(data);
}

#ifndef NO_QT_SUPPORT
XmlStreamReader::XmlStreamReader(const QByteArray& data)
{
    m_xml = new Xml();
    ByteArray ba = ByteArray::fromQByteArrayNoCopy(data);
    setData(ba);
}

#endif

XmlStreamReader::~XmlStreamReader()
{
    delete m_xml;
}

void XmlStreamReader::setData(const ByteArray& data_)
{
    m_xml->doc.Clear();
    m_xml->customErr.clear();
    m_token = TokenType::Invalid;

    if (data_.size() < 4) {
        m_xml->err = XML_ERROR_EMPTY_DOCUMENT;
        LOGE() << m_xml->doc.ErrorIDToName(m_xml->err);
        return;
    }

    UtfCodec::Encoding enc = UtfCodec::xmlEncoding(data_);
    if (enc == UtfCodec::Encoding::Unknown) {
        m_xml->err = XML_CAN_NOT_CONVERT_TEXT;
        LOGE() << "unknown encoding";
        return;
    }

    if (enc == UtfCodec::Encoding::UTF_16BE) {
        m_xml->err = XML_CAN_NOT_CONVERT_TEXT;
        LOGE() << "unsupported encoding UTF-16BE";
        return;
    }

    ByteArray data = data_; // no copy, implicit sharing
    if (enc == UtfCodec::Encoding::UTF_16LE) {
        String u16 = String::fromUtf16LE(data_);
        data = u16.toUtf8();
    }

    m_xml->err = m_xml->doc.Parse(reinterpret_cast<const char*>(data.constData()), data.size());

    if (m_xml->err == XML_SUCCESS) {
        m_token = TokenType::NoToken;
    } else {
        LOGE() << m_xml->doc.ErrorIDToName(m_xml->err);
    }
}

bool XmlStreamReader::readNextStartElement()
{
    while (readNext() != Invalid) {
        if (isEndElement()) {
            return false;
        } else if (isStartElement()) {
            return true;
        }
    }
    return false;
}

bool XmlStreamReader::atEnd() const
{
    return m_token == TokenType::EndDocument || m_token == TokenType::Invalid;
}

static XmlStreamReader::TokenType resolveToken(XMLNode* n, bool isStartElement)
{
    if (n->ToElement()) {
        return isStartElement ? XmlStreamReader::TokenType::StartElement : XmlStreamReader::TokenType::EndElement;
    } else if (n->ToText()) {
        return XmlStreamReader::TokenType::Characters;
    } else if (n->ToComment()) {
        return XmlStreamReader::TokenType::Comment;
    } else if (n->ToDeclaration()) {
        return XmlStreamReader::TokenType::StartDocument;
    } else if (n->ToDocument()) {
        return XmlStreamReader::TokenType::EndDocument;
    } else if (n->ToUnknown()) {
        return XmlStreamReader::TokenType::DTD;
    }
    return XmlStreamReader::TokenType::Unknown;
}

static std::pair<XMLNode*, XmlStreamReader::TokenType> resolveNode(XMLNode* currentNode, XmlStreamReader::TokenType currentToken)
{
    if (currentToken == XmlStreamReader::TokenType::StartElement) {
        XMLNode* child = currentNode->FirstChild();
        if (child) {
            return { child, resolveToken(child, true) };
        }

        XMLNode* sibling = currentNode->NextSibling();
        if (!sibling || sibling->ToElement() || sibling->ToText()) {
            return { currentNode, XmlStreamReader::TokenType::EndElement };
        }
    }

    XMLNode* sibling = currentNode->NextSibling();
    if (sibling) {
        return { sibling, resolveToken(sibling, true) };
    }

    XMLNode* parent = currentNode->Parent();
    if (parent) {
        return { parent, resolveToken(parent, false) };
    }

    return { nullptr, XmlStreamReader::TokenType::EndDocument };
}

XmlStreamReader::TokenType XmlStreamReader::readNext()
{
    if (m_token == TokenType::Invalid) {
        return m_token;
    }

    if (m_xml->err != XML_SUCCESS || m_token == EndDocument) {
        m_xml->node = nullptr;
        m_token = TokenType::Invalid;
        return m_token;
    }

    if (!m_xml->node) {
        m_xml->node = m_xml->doc.FirstChild();
        m_token = m_xml->node->ToDeclaration() ? TokenType::StartDocument : resolveToken(m_xml->node, true);
        return m_token;
    }

    std::pair<XMLNode*, XmlStreamReader::TokenType> p = resolveNode(m_xml->node, m_token);

    m_xml->node = p.first;
    m_token = p.second;

    if (m_token == XmlStreamReader::TokenType::DTD) {
        tryParseEntity(m_xml);
    }

    return m_token;
}

void XmlStreamReader::tryParseEntity(Xml* xml)
{
    static const char* ENTITY = { "ENTITY" };

    const char* str = xml->node->Value();
    if (std::strncmp(str, ENTITY, 6) == 0) {
        String val = String::fromUtf8(str);
        StringList list = val.split(' ');
        if (list.size() == 3) {
            String name = list.at(1);
            String val2 = list.at(2);
            m_entities[u'&' + name + u';'] = val2.mid(1, val2.size() - 2);
        } else {
            LOGW() << "unknown ENTITY: " << val;
        }
    }
}

String XmlStreamReader::nodeValue(Xml* xml) const
{
    String str = String::fromUtf8(xml->node->Value());
    if (!m_entities.empty()) {
        for (const auto& p : m_entities) {
            str.replace(p.first, p.second);
        }
    }
    return str;
}

XmlStreamReader::TokenType XmlStreamReader::tokenType() const
{
    return m_token;
}

AsciiStringView XmlStreamReader::tokenString() const
{
    switch (m_token) {
    case TokenType::NoToken: return "NoToken";
    case TokenType::Invalid: return "Invalid";
    case TokenType::StartDocument: return "StartDocument";
    case TokenType::EndDocument: return "EndDocument";
    case TokenType::StartElement: return "StartElement";
    case TokenType::EndElement: return "EndElement";
    case TokenType::Characters: return "Characters";
    case TokenType::Comment: return "Comment";
    case TokenType::DTD: return "DTD";
    case TokenType::Unknown: return "Unknown";
    }
    return AsciiStringView();
}

bool XmlStreamReader::isWhitespace() const
{
    return false;
}

void XmlStreamReader::skipCurrentElement()
{
    int depth = 1;
    while (depth && readNext() != Invalid) {
        if (isEndElement()) {
            --depth;
        } else if (isStartElement()) {
            ++depth;
        }
    }
}

AsciiStringView XmlStreamReader::name() const
{
    return (m_xml->node && m_xml->node->ToElement()) ? m_xml->node->Value() : AsciiStringView();
}

bool XmlStreamReader::hasAttribute(const char* name) const
{
    if (m_token != TokenType::StartElement) {
        return false;
    }

    XMLElement* e = m_xml->node->ToElement();
    if (!e) {
        return false;
    }
    return e->FindAttribute(name) != nullptr;
}

String XmlStreamReader::attribute(const char* name) const
{
    if (m_token != TokenType::StartElement) {
        return String();
    }

    XMLElement* e = m_xml->node->ToElement();
    if (!e) {
        return String();
    }
    return String::fromUtf8(e->Attribute(name));
}

String XmlStreamReader::attribute(const char* name, const String& def) const
{
    return hasAttribute(name) ? attribute(name) : def;
}

AsciiStringView XmlStreamReader::asciiAttribute(const char* name) const
{
    if (m_token != TokenType::StartElement) {
        return AsciiStringView();
    }

    XMLElement* e = m_xml->node->ToElement();
    if (!e) {
        return AsciiStringView();
    }
    return e->Attribute(name);
}

AsciiStringView XmlStreamReader::asciiAttribute(const char* name, const AsciiStringView& def) const
{
    return hasAttribute(name) ? asciiAttribute(name) : def;
}

int XmlStreamReader::intAttribute(const char* name) const
{
    return asciiAttribute(name).toInt();
}

int XmlStreamReader::intAttribute(const char* name, int def) const
{
    return hasAttribute(name) ? intAttribute(name) : def;
}

double XmlStreamReader::doubleAttribute(const char* name) const
{
    return asciiAttribute(name).toDouble();
}

double XmlStreamReader::doubleAttribute(const char* name, double def) const
{
    return hasAttribute(name) ? doubleAttribute(name) : def;
}

std::vector<XmlStreamReader::Attribute> XmlStreamReader::attributes() const
{
    std::vector<Attribute> attrs;
    if (m_token != TokenType::StartElement) {
        return attrs;
    }

    XMLElement* e = m_xml->node->ToElement();
    if (!e) {
        return attrs;
    }

    for (const XMLAttribute* xa = e->FirstAttribute(); xa; xa = xa->Next()) {
        Attribute a;
        a.name = xa->Name();
        a.value = String::fromUtf8(xa->Value());
        attrs.push_back(std::move(a));
    }
    return attrs;
}

String XmlStreamReader::text() const
{
    if (m_xml->node && (m_xml->node->ToText() || m_xml->node->ToComment())) {
        return nodeValue(m_xml);
    }
    return String();
}

AsciiStringView XmlStreamReader::asciiText() const
{
    if (m_xml->node && (m_xml->node->ToText() || m_xml->node->ToComment())) {
        return m_xml->node->Value();
    }
    return AsciiStringView();
}

String XmlStreamReader::readText()
{
    if (isStartElement()) {
        String result;
        while (1) {
            switch (readNext()) {
            case Characters:
                result = nodeValue(m_xml);
                break;
            case EndElement:
                return result;
            case Comment:
                break;
            case StartElement:
                break;
            default:
                break;
            }
        }
    }
    return String();
}

AsciiStringView XmlStreamReader::readAsciiText()
{
    if (isStartElement()) {
        AsciiStringView result;
        while (1) {
            switch (readNext()) {
            case Characters:
                result = AsciiStringView(m_xml->node->Value());
                break;
            case EndElement:
                return result;
            case Comment:
                break;
            case StartElement:
                break;
            default:
                break;
            }
        }
    }
    return AsciiStringView();
}

int XmlStreamReader::readInt(bool* ok, int base)
{
    AsciiStringView s = readAsciiText();
    return s.toInt(ok, base);
}

double XmlStreamReader::readDouble(bool* ok)
{
    AsciiStringView s = readAsciiText();
    return s.toDouble(ok);
}

int64_t XmlStreamReader::lineNumber() const
{
    return m_xml->doc.ErrorLineNum();
}

int64_t XmlStreamReader::columnNumber() const
{
    return 0;
}

XmlStreamReader::Error XmlStreamReader::error() const
{
    if (!m_xml->customErr.isEmpty()) {
        return CustomError;
    }

    XMLError err = m_xml->doc.ErrorID();
    if (err == XML_SUCCESS) {
        return NoError;
    }

    return NotWellFormedError;
}

bool XmlStreamReader::isError() const
{
    return error() != NoError;
}

String XmlStreamReader::errorString() const
{
    if (!m_xml->customErr.empty()) {
        return m_xml->customErr;
    }
    return String::fromUtf8(m_xml->doc.ErrorStr());
}

void XmlStreamReader::raiseError(const String& message)
{
    m_xml->customErr = message;
}
