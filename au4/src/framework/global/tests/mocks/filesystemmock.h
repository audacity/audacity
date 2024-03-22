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
#ifndef MU_SYSTEM_FILESYSTEMMOCK_H
#define MU_SYSTEM_FILESYSTEMMOCK_H

#include <gmock/gmock.h>

#include "io/ifilesystem.h"

namespace mu::io {
class FileSystemMock : public IFileSystem
{
public:
    MOCK_METHOD(Ret, exists, (const io::path_t&), (const, override));
    MOCK_METHOD(Ret, remove, (const io::path_t&, bool onlyIfEmpty), (override));
    MOCK_METHOD(Ret, clear, (const io::path_t&), (override));
    MOCK_METHOD(Ret, copy, (const io::path_t& src, const io::path_t& dst, bool replace), (override));
    MOCK_METHOD(Ret, move, (const io::path_t& src, const io::path_t& dst, bool replace), (override));

    MOCK_METHOD(EntryType, entryType, (const io::path_t& path), (const, override));

    MOCK_METHOD(RetVal<uint64_t>, fileSize, (const io::path_t& path), (const, override));

    MOCK_METHOD(RetVal<ByteArray>, readFile, (const io::path_t&), (const, override));
    MOCK_METHOD(Ret, readFile, (const io::path_t& filePath, ByteArray & data), (const, override));
    MOCK_METHOD(Ret, writeFile, (const io::path_t& filePath, const ByteArray& data), (const, override));

    MOCK_METHOD(Ret, makePath, (const io::path_t&), (const, override));

    MOCK_METHOD(RetVal<io::paths_t>, scanFiles, (const io::path_t&, const std::vector<std::string>&, ScanMode), (const, override));

    MOCK_METHOD(void, setAttribute, (const io::path_t& path, Attribute attribute), (const, override));
    MOCK_METHOD(bool, setPermissionsAllowedForAll, (const io::path_t& path), (const, override));

    MOCK_METHOD(io::path_t, canonicalFilePath, (const io::path_t& filePath), (const, override));
    MOCK_METHOD(io::path_t, absolutePath, (const io::path_t& filePath), (const, override));
    MOCK_METHOD(io::path_t, absoluteFilePath, (const io::path_t& filePath), (const, override));
    MOCK_METHOD(DateTime, birthTime, (const io::path_t& filePath), (const, override));
    MOCK_METHOD(DateTime, lastModified, (const io::path_t& filePath), (const, override));
    MOCK_METHOD(Ret, isWritable, (const io::path_t& filePath), (const, override));
};
}

#endif // MU_SYSTEM_FILESYSTEMMOCK_H
