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

#include "xmlwriter.h"

#include <QXmlStreamWriter>
#include <QFile>

using namespace mu::deprecated;
using namespace mu::io;

XmlWriter::XmlWriter(const io::path_t& path)
{
    m_device = std::make_unique<QFile>(path.toString());
    m_device->open(QIODevice::WriteOnly);

    initWriter(m_device.get());
}

XmlWriter::XmlWriter(QIODevice* device)
{
    initWriter(device);
}

void XmlWriter::initWriter(QIODevice* device)
{
    m_writer = std::make_unique<QXmlStreamWriter>(device);
    m_writer->setAutoFormatting(true);
}

XmlWriter::~XmlWriter()
{
    if (m_device) {
        m_device->close();
    }
}

void XmlWriter::writeStartDocument(std::string_view version)
{
    if (version.empty()) {
        m_writer->writeStartDocument();
        return;
    }

    m_writer->writeStartDocument(version.data());
}

void XmlWriter::writeEndDocument()
{
    m_writer->writeEndDocument();
}

void XmlWriter::writeStartElement(std::string_view name)
{
    m_writer->writeStartElement(name.data());
}

void XmlWriter::writeEndElement()
{
    m_writer->writeEndElement();
}

void XmlWriter::writeAttribute(std::string_view name, const std::string& value)
{
    m_writer->writeAttribute(name.data(), QString::fromStdString(value));
}

void XmlWriter::writeCharacters(const std::string& text)
{
    m_writer->writeCharacters(QString::fromStdString(text));
}

void XmlWriter::writeTextElement(std::string_view name, const std::string& text)
{
    m_writer->writeTextElement(name.data(), QString::fromStdString(text));
}

bool XmlWriter::success() const
{
    return !m_writer->hasError();
}
