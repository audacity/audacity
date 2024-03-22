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
#include "textstream.h"
#include <sstream>

using namespace mu;

static constexpr int TEXTSTREAM_BUFFERSIZE = 16384;

TextStream::TextStream(io::IODevice* device)
    : m_device(device)
{
}

TextStream::~TextStream()
{
    flush();
}

void TextStream::setDevice(io::IODevice* device)
{
    m_device = device;
}

void TextStream::flush()
{
    if (m_device && m_device->isOpen()) {
        m_device->write(m_buf);
        m_buf.clear();
    }
}

TextStream& TextStream::operator<<(char ch)
{
    write(&ch, 1);
    return *this;
}

TextStream& TextStream::operator<<(int val)
{
    std::stringstream ss;
    ss << val;
    write(ss.str().c_str(), ss.str().size());
    return *this;
}

TextStream& TextStream::operator<<(unsigned int val)
{
    std::stringstream ss;
    ss << val;
    write(ss.str().c_str(), ss.str().size());
    return *this;
}

TextStream& TextStream::operator<<(double val)
{
    std::stringstream ss;
    ss << val;
    write(ss.str().c_str(), ss.str().size());
    return *this;
}

TextStream& TextStream::operator<<(signed long int val)
{
    std::stringstream ss;
    ss << val;
    write(ss.str().c_str(), ss.str().size());
    return *this;
}

TextStream& TextStream::operator<<(unsigned long int val)
{
    std::stringstream ss;
    ss << val;
    write(ss.str().c_str(), ss.str().size());
    return *this;
}

TextStream& TextStream::operator<<(signed long long val)
{
    std::stringstream ss;
    ss << val;
    write(ss.str().c_str(), ss.str().size());
    return *this;
}

TextStream& TextStream::operator<<(unsigned long long val)
{
    std::stringstream ss;
    ss << val;
    write(ss.str().c_str(), ss.str().size());
    return *this;
}

TextStream& TextStream::operator<<(const char* s)
{
    write(s, std::strlen(s));
    return *this;
}

TextStream& TextStream::operator<<(const std::string& str)
{
    write(str.c_str(), str.size());
    return *this;
}

#ifndef NO_QT_SUPPORT
TextStream& TextStream::operator<<(const QString& s)
{
    QByteArray ba = s.toUtf8();
    write(ba.constData(), ba.length());
    return *this;
}

#endif

TextStream& TextStream::operator<<(const ByteArray& b)
{
    write(reinterpret_cast<const char*>(b.constData()), b.size());
    return *this;
}

TextStream& TextStream::operator<<(const AsciiStringView& s)
{
    write(s.ascii(), s.size());
    return *this;
}

TextStream& TextStream::operator<<(const String& s)
{
    ByteArray b = s.toUtf8();
    write(reinterpret_cast<const char*>(b.constData()), b.size());
    return *this;
}

void TextStream::write(const char* ch, size_t len)
{
    m_buf.push_back(reinterpret_cast<const uint8_t*>(ch), len);
    if (m_device && m_buf.size() > TEXTSTREAM_BUFFERSIZE) {
        flush();
    }
}
