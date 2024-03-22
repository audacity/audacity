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

#ifndef MU_GLOBAL_XMLREADER_H
#define MU_GLOBAL_XMLREADER_H

#include <memory>
#include <QIODevice>

#include "global/io/path.h"

//! NOTE This is class is deprecated, please use serialization/xmlstreamreader.h or serialization/xmldom.h

class QXmlStreamReader;
class QByteArray;

namespace mu::deprecated {
class XmlReader
{
public:
    XmlReader(const io::path_t& path);
    XmlReader(QIODevice* device);
    XmlReader(const QByteArray& bytes);
    ~XmlReader();

    bool readNextStartElement();
    bool canRead() const;
    void skipCurrentElement();
    std::string tagName() const;

    enum TokenType {
        Unknown,
        StartDocument,
        EndDocument,
        StartElement,
        EndElement,
        Characters,
        Comment
    };

    TokenType readNext();
    TokenType tokenType() const;

    int intAttribute(std::string_view name, int defaultValue = 0) const;
    double doubleAttribute(std::string_view name, double defaultValue = 0.) const;
    std::string attribute(std::string_view name) const;
    bool hasAttribute(std::string_view name) const;

    enum ReadStringBehavior {
        ErrorOnUnexpectedElement,
        IncludeChildElements,
        SkipChildElements
    };

    std::string readString(ReadStringBehavior behavior = ErrorOnUnexpectedElement);
    int readInt();
    double readDouble();

    bool success() const;
    std::string error() const;

private:
    QString readElementText(ReadStringBehavior behavior = ErrorOnUnexpectedElement);
    QStringView attributeValue(std::string_view name) const;

    std::unique_ptr<QIODevice> m_device;
    std::unique_ptr<QXmlStreamReader> m_reader;
};
}

#endif // MU_GLOBAL_XMLREADER_H
