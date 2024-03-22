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
#ifndef MU_GLOBAL_XMLDOM_H
#define MU_GLOBAL_XMLDOM_H

#include <memory>

#include "types/bytearray.h"
#include "types/string.h"

namespace mu {
struct XmlDomData;

class XmlDomElement;
class XmlDomNode
{
public:

    XmlDomNode() = default;

    bool isNull() const;
    String nodeName() const;

    bool hasChildNodes() const;
    XmlDomNode firstChild() const;
    XmlDomElement firstChildElement(const char* name) const;
    XmlDomNode nextSibling() const;

    bool hasAttribute(const char* name) const;
    String attribute(const char* name) const;

    XmlDomElement toElement() const;

protected:
    friend class XmlDomDocument;
    friend class XmlDomElement;

    XmlDomNode(const std::shared_ptr<XmlDomData>& xml, uintptr_t node);

    std::shared_ptr<XmlDomData> m_xml = nullptr;
    uintptr_t m_node = 0;
};

class XmlDomElement : public XmlDomNode
{
public:

    String text() const;

private:

    friend class XmlDomDocument;
    friend class XmlDomNode;

    XmlDomElement(const std::shared_ptr<XmlDomData>& data, uintptr_t node);
};

class XmlDomDocument
{
public:
    XmlDomDocument();

    void setContent(const ByteArray& data);

    XmlDomElement rootElement() const;

    bool hasError() const;
    String errorString() const;

private:
    std::shared_ptr<XmlDomData> m_xml = nullptr;
};
}

#endif // MU_GLOBAL_XMLDOM_H
