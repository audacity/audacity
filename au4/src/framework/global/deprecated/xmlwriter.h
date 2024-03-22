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

#ifndef MU_GLOBAL_XMLWRITER_H
#define MU_GLOBAL_XMLWRITER_H

#include <memory>
#include <QIODevice>

#include "global/io/path.h"

//! NOTE This is class is deprecated, please use serialization/xmlstreamwriter.h or serialization/xmldom.h

class QXmlStreamWriter;

namespace mu::deprecated {
class XmlWriter
{
public:
    XmlWriter(const io::path_t& path);
    XmlWriter(QIODevice* device);
    ~XmlWriter();

    void writeStartDocument(std::string_view version = std::string_view());
    void writeEndDocument();

    void writeStartElement(std::string_view name);
    void writeEndElement();

    void writeAttribute(std::string_view name, const std::string& value);
    void writeCharacters(const std::string& text);
    void writeTextElement(std::string_view name, const std::string& text);

    bool success() const;

private:
    void initWriter(QIODevice* device);

    std::unique_ptr<QIODevice> m_device;
    std::unique_ptr<QXmlStreamWriter> m_writer;
};
}

#endif // MU_GLOBAL_XMLWRITER_H
