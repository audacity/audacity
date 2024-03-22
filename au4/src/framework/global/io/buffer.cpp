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
#include "buffer.h"

#include <cstring>

using namespace mu;
using namespace mu::io;

Buffer::Buffer()
{
    m_ref = &m_ba;
}

Buffer::Buffer(size_t size)
{
    m_ba.resize(size);
    m_ref = &m_ba;
}

Buffer::Buffer(const uint8_t* data, size_t size)
{
    m_ba = ByteArray(data, size);
    m_ref = &m_ba;
}

Buffer::Buffer(ByteArray* ba)
    : m_ref(ba)
{
    if (!m_ref) {
        m_ref = &m_ba;
    }
}

Buffer::Buffer(ByteArray&& ba)
{
    m_ba = std::move(ba);
    m_ref = &m_ba;
}

const ByteArray& Buffer::data() const
{
    return *m_ref;
}

bool Buffer::doOpen(OpenMode)
{
    return true;
}

size_t Buffer::dataSize() const
{
    return m_ref->size();
}

const uint8_t* Buffer::rawData() const
{
    return m_ref->constData();
}

bool Buffer::resizeData(size_t size)
{
    m_ref->resize(size);
    return true;
}

size_t Buffer::writeData(const uint8_t* data, size_t len)
{
    std::memcpy(m_ref->data() + pos(), data, len);
    return len;
}
