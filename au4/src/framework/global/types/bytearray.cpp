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
#include "types/bytearray.h"

#include <cstring>
#include <cassert>

using namespace mu;

ByteArray::ByteArray()
{
    m_data = std::make_shared<Data>();
    m_data->resize(1);
    m_data->operator [](0) = 0;
}

ByteArray::ByteArray(const uint8_t* data, size_t size)
{
    m_data = std::make_shared<Data>();
    m_data->resize(size + 1);
    m_data->operator [](size) = 0;
    std::memcpy(m_data->data(), data, size);
}

ByteArray::ByteArray(const char* str, size_t size)
{
    size = (size == static_cast<size_t>(-1)) ? std::strlen(str) : size;
    m_data = std::make_shared<Data>();
    m_data->resize(size + 1);
    m_data->operator [](size) = 0;
    std::memcpy(m_data->data(), str, size);
}

ByteArray::ByteArray(size_t size)
{
    m_data = std::make_shared<Data>();
    m_data->resize(size + 1);
    m_data->operator [](size) = 0;
}

ByteArray ByteArray::fromRawData(const uint8_t* data, size_t size)
{
    ByteArray ba;
    ba.m_raw.size = size;
    ba.m_raw.data = data;
    return ba;
}

ByteArray ByteArray::fromRawData(const char* data, size_t size)
{
    return fromRawData(reinterpret_cast<const uint8_t*>(data), size);
}

uint8_t* ByteArray::data()
{
    detach();
    return m_data->data();
}

const uint8_t* ByteArray::constData() const
{
    if (m_raw.data) {
        return m_raw.data;
    }

    return m_data->data();
}

const char* ByteArray::constChar() const
{
    return reinterpret_cast<const char*>(constData());
}

size_t ByteArray::size() const
{
    if (m_raw.data) {
        return m_raw.size;
    }

    return m_data->empty() ? 0 : (m_data->size() - 1);
}

void ByteArray::detach()
{
    if (!m_data) {
        return;
    }

    if (m_raw.data) {
        m_data->resize(m_raw.size + 1);
        m_data->operator [](m_raw.size) = 0;
        std::memcpy(m_data->data(), m_raw.data, m_raw.size);
        m_raw.data = nullptr;
        return;
    }

    if (m_data.use_count() == 1) {
        return;
    }

    m_data = std::make_shared<Data>(*m_data);
}

bool ByteArray::operator==(const ByteArray& other) const
{
    if (size() != other.size()) {
        return false;
    }
    return std::memcmp(constData(), other.constData(), size()) == 0;
}

bool ByteArray::empty() const
{
    return size() == 0;
}

void ByteArray::reserve(size_t nsize)
{
    if (nsize + 1 <= m_data->capacity()) {
        return;
    }

    detach();
    m_data->reserve(nsize + 1);
}

void ByteArray::resize(size_t nsize)
{
    if (nsize == size()) {
        return;
    }

    detach();
    m_data->resize(nsize + 1);
    m_data->operator [](nsize) = 0;
}

void ByteArray::truncate(size_t pos)
{
    if (pos >= size()) {
        return;
    }
    resize(pos);
}

void ByteArray::clear()
{
    resize(0);
}

ByteArray& ByteArray::insert(size_t pos, uint8_t b)
{
    if (pos > size()) {
        return *this;
    }

    detach();
    m_data->insert(m_data->begin() + pos, b);
    return *this;
}

void ByteArray::push_back(uint8_t b)
{
    insert(size(), b);
}

void ByteArray::push_back(const uint8_t* b, size_t len)
{
    detach();
    size_t start = size();
    size_t nsize = start + len;
    Data& data = *m_data.get();
    data.resize(nsize + 1);
    data[nsize] = 0;
    for (size_t i = 0; i < len; ++i) {
        data[start + i] = b[i];
    }
}

void ByteArray::push_back(const ByteArray& ba)
{
    push_back(ba.constData(), ba.size());
}

uint8_t ByteArray::at(size_t pos) const
{
    assert(pos < size());
    if (pos < size()) {
        return constData()[pos];
    }
    return 0;
}

uint8_t ByteArray::operator[](size_t pos) const
{
    return at(pos);
}

uint8_t& ByteArray::operator[](size_t pos)
{
    detach();

    assert(pos < size());
    if (pos < size()) {
        return m_data->operator [](pos);
    }

    static uint8_t dummy;
    return dummy;
}

ByteArray ByteArray::left(size_t len) const
{
    return ByteArray(constData(), len);
}

ByteArray ByteArray::right(size_t len) const
{
    return ByteArray(&(constData()[size() - len]), len);
}
