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

#ifndef MU_IO_IODEVICE_H
#define MU_IO_IODEVICE_H

#include <cstdint>
#include <string>
#include <map>

#include "global/types/bytearray.h"

namespace mu::io {
class IODevice
{
public:

    enum OpenMode {
        Unknown, ReadOnly, WriteOnly, ReadWrite, Append
    };

    IODevice() = default;
    virtual ~IODevice() = default;

    bool open(OpenMode m = ReadOnly);
    void close();

    bool isOpen() const;
    OpenMode openMode() const;
    bool isReadable() const;
    bool isWriteable() const;

    size_t size() const;
    size_t pos() const;

    bool seek(size_t pos);

    size_t read(uint8_t* data, size_t len);
    ByteArray read(size_t count);
    ByteArray readAll();

    const uint8_t* readData();

    size_t write(const uint8_t* data, size_t len);
    size_t write(const ByteArray& ba);

#ifndef NO_QT_SUPPORT
    size_t write(const QByteArray& ba);
#endif

    std::string meta(const std::string& key) const;
    void setMeta(const std::string& key, const std::string& val);

    bool hasError() const;
    int error() const;
    std::string errorString() const;

protected:

    virtual bool doOpen(OpenMode m) = 0;
    virtual size_t dataSize() const = 0;
    virtual const uint8_t* rawData() const = 0;
    virtual bool resizeData(size_t size) = 0;
    virtual size_t writeData(const uint8_t* data, size_t len) = 0;

    bool isOpenModeReadable() const;
    bool isOpenModeWriteable() const;

    void setError(int error, const std::string& errorString);

private:

    const uint8_t* cdataOffsetted() const;

    OpenMode m_mode = OpenMode::Unknown;
    size_t m_pos = 0;
    std::map<std::string, std::string> m_meta;

    int m_error = 0;
    std::string m_errorString;
};
}

#endif // MU_IO_IODEVICE_H
