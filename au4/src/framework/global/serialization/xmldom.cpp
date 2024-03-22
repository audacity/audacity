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
#include "xmldom.h"

#include "../thirdparty/tinyxml/tinyxml2.h"

#include "log.h"

using namespace mu;

struct mu::XmlDomData
{
    tinyxml2::XMLDocument doc;
    tinyxml2::XMLError err;
};

// ================================================
// XmlDomNode
// ================================================
static inline const tinyxml2::XMLNode* node_const(uintptr_t p)
{
    return reinterpret_cast<const tinyxml2::XMLNode*>(p);
}

XmlDomNode::XmlDomNode(const std::shared_ptr<XmlDomData>& xml, uintptr_t node)
    : m_xml(xml), m_node(node)
{
}

bool XmlDomNode::isNull() const
{
    return m_node ? false : true;
}

String XmlDomNode::nodeName() const
{
    return m_node ? String::fromUtf8(node_const(m_node)->Value()) : String();
}

bool XmlDomNode::hasChildNodes() const
{
    return m_node ? !node_const(m_node)->NoChildren() : false;
}

XmlDomNode XmlDomNode::firstChild() const
{
    if (!m_node) {
        return XmlDomNode(m_xml, 0);
    }
    const tinyxml2::XMLNode* n = node_const(m_node)->FirstChild();
    return XmlDomNode(m_xml, reinterpret_cast<uintptr_t>(n));
}

XmlDomElement XmlDomNode::firstChildElement(const char* name) const
{
    if (!m_node) {
        return XmlDomElement(m_xml, 0);
    }

    const tinyxml2::XMLElement* n = node_const(m_node)->FirstChildElement(name);
    return XmlDomElement(m_xml, reinterpret_cast<uintptr_t>(n));
}

XmlDomNode XmlDomNode::nextSibling() const
{
    if (!m_node) {
        return XmlDomNode(m_xml, 0);
    }
    const tinyxml2::XMLNode* n = node_const(m_node)->NextSibling();
    return XmlDomNode(m_xml, reinterpret_cast<uintptr_t>(n));
}

bool XmlDomNode::hasAttribute(const char* name) const
{
    if (!m_node) {
        return false;
    }

    const tinyxml2::XMLElement* e = node_const(m_node)->ToElement();
    if (!e) {
        return false;
    }
    return e->FindAttribute(name) != nullptr;
}

String XmlDomNode::attribute(const char* name) const
{
    if (!m_node) {
        return String();
    }

    const tinyxml2::XMLElement* e = node_const(m_node)->ToElement();
    if (!e) {
        return String();
    }
    return String::fromUtf8(e->Attribute(name));
}

XmlDomElement XmlDomNode::toElement() const
{
    const tinyxml2::XMLElement* e = node_const(m_node)->ToElement();
    return XmlDomElement(m_xml, reinterpret_cast<uintptr_t>(e));
}

// ================================================
// XmlDomElement
// ================================================
static inline const tinyxml2::XMLElement* el_const(uintptr_t p)
{
    return reinterpret_cast<const tinyxml2::XMLNode*>(p)->ToElement();
}

XmlDomElement::XmlDomElement(const std::shared_ptr<XmlDomData>& data, uintptr_t node)
    : XmlDomNode(data, node)
{
}

String XmlDomElement::text() const
{
    const tinyxml2::XMLElement* e = el_const(m_node);
    if (!e) {
        return String();
    }

    String result;
    for (const tinyxml2::XMLNode* n = e->FirstChild(); n != nullptr; n = n->NextSibling()) {
        const tinyxml2::XMLText* t = n->ToText();
        if (t) {
            result += String::fromUtf8(t->Value());
        }
    }

    return result;
}

// ================================================
// XmlDomDocument
// ================================================

XmlDomDocument::XmlDomDocument()
{
    m_xml = std::make_shared<XmlDomData>();
}

void XmlDomDocument::setContent(const ByteArray& data)
{
    m_xml->doc.Clear();
    m_xml->err = m_xml->doc.Parse(reinterpret_cast<const char*>(data.constData()), data.size());

    if (m_xml->err != tinyxml2::XML_SUCCESS) {
        LOGE() << errorString();
    }
}

XmlDomElement XmlDomDocument::rootElement() const
{
    const tinyxml2::XMLElement* e = m_xml->doc.FirstChildElement();
    return XmlDomElement(m_xml, reinterpret_cast<uintptr_t>(e));
}

bool XmlDomDocument::hasError() const
{
    return m_xml->err == tinyxml2::XML_SUCCESS;
}

String XmlDomDocument::errorString() const
{
    return String::fromUtf8(m_xml->doc.ErrorStr());
}
