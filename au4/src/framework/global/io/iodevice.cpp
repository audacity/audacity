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
#include "iodevice.h"

#include <cstring>

#ifndef NO_QT_SUPPORT
#include <QByteArray>
#endif

#include "log.h"

using namespace mu;
using namespace mu::io;

bool IODevice::open(IODevice::OpenMode mode)
{
    if (isOpen()) {
        return true;
    }

    bool ok = doOpen(mode);
    if (ok) {
        m_mode = mode;
        m_pos = mode == Append ? dataSize() : 0;
    }

    return ok;
}

void IODevice::close()
{
    m_mode = Unknown;
}

bool IODevice::isOpen() const
{
    return m_mode != OpenMode::Unknown;
}

IODevice::OpenMode IODevice::openMode() const
{
    return m_mode;
}

bool IODevice::isOpenModeReadable() const
{
    return m_mode == OpenMode::ReadOnly || m_mode == OpenMode::ReadWrite;
}

bool IODevice::isReadable() const
{
    return isOpenModeReadable() && m_pos < size();
}

bool IODevice::isOpenModeWriteable() const
{
    return m_mode == OpenMode::WriteOnly || m_mode == OpenMode::ReadWrite || m_mode == OpenMode::Append;
}

void IODevice::setError(int error, const std::string& errorString)
{
    m_error = error;
    m_errorString = errorString;
}

bool IODevice::isWriteable() const
{
    return isOpenModeWriteable();
}

size_t IODevice::size() const
{
    IF_ASSERT_FAILED(isOpen()) {
        return 0;
    }

    return dataSize();
}

size_t IODevice::pos() const
{
    return m_pos;
}

bool IODevice::seek(size_t pos)
{
    IF_ASSERT_FAILED(isOpen()) {
        return false;
    }

    if (pos <= size()) {
        m_pos = pos;
        return true;
    }

    bool ok = resizeData(pos);
    if (ok) {
        m_pos = pos;
    }

    return ok;
}

size_t IODevice::read(uint8_t* data, size_t len)
{
    IF_ASSERT_FAILED(isOpenModeReadable()) {
        return 0;
    }

    IF_ASSERT_FAILED(m_pos <= size()) {
        return 0;
    }

    if (m_pos == size()) {
        memset(data, 0, len);
        return 0;
    }

    size_t left = size() - m_pos;
    if (left < len) {
        len = left;
    }

    std::memcpy(data, cdataOffsetted(), len);

    m_pos += len;
    return len;
}

ByteArray IODevice::read(size_t len)
{
    IF_ASSERT_FAILED(isOpenModeReadable()) {
        return ByteArray();
    }

    IF_ASSERT_FAILED(m_pos <= size()) {
        return ByteArray();
    }

    if (m_pos == size()) {
        return ByteArray();
    }

    size_t left = size() - m_pos;
    if (left < len) {
        len = left;
    }

    ByteArray result(cdataOffsetted(), len);

    m_pos += len;

    return result;
}

const uint8_t* IODevice::cdataOffsetted() const
{
    const uint8_t* d = rawData();
    IF_ASSERT_FAILED(d) {
        return nullptr;
    }
    return d + m_pos;
}

ByteArray IODevice::readAll()
{
    return read(size());
}

const uint8_t* IODevice::readData()
{
    IF_ASSERT_FAILED(isOpen()) {
        return nullptr;
    }
    return rawData();
}

size_t IODevice::write(const uint8_t* data, size_t len)
{
    IF_ASSERT_FAILED(isOpenModeWriteable()) {
        return 0;
    }

    size_t left = size() - m_pos;
    if (left < len) {
        bool ok = resizeData(m_pos + len);
        if (!ok) {
            LOGE() << "failed resize data";
            return 0;
        }
    }

    len = writeData(data, len);
    m_pos += len;

    return len;
}

size_t IODevice::write(const ByteArray& ba)
{
    return write(ba.constData(), ba.size());
}

#ifndef NO_QT_SUPPORT
size_t IODevice::write(const QByteArray& ba)
{
    return write(reinterpret_cast<const uint8_t*>(ba.constData()), ba.size());
}

#endif

std::string IODevice::meta(const std::string& key) const
{
    auto it = m_meta.find(key);
    if (it != m_meta.end()) {
        return it->second;
    }
    return std::string();
}

void IODevice::setMeta(const std::string& key, const std::string& val)
{
    m_meta[key] = val;
}

bool IODevice::hasError() const
{
    return m_error != 0;
}

int IODevice::error() const
{
    return m_error;
}

std::string IODevice::errorString() const
{
    return m_errorString;
}
