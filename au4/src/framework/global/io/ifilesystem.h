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
#ifndef MU_IO_IFILESYSTEM_H
#define MU_IO_IFILESYSTEM_H

#include "global/modularity/imoduleinterface.h"
#include "global/types/bytearray.h"
#include "global/types/datetime.h"
#include "global/types/retval.h"

#include "path.h"
#include "ioenums.h"

namespace mu::io {
class IFileSystem : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IFileSystem)

public:
    virtual ~IFileSystem() = default;

    virtual Ret exists(const io::path_t& path) const = 0;
    virtual Ret remove(const io::path_t& path, bool onlyIfEmpty = false) = 0; // remove file or dir
    virtual Ret clear(const io::path_t& path) = 0; // clear dir
    virtual Ret copy(const io::path_t& src, const io::path_t& dst, bool replace = false) = 0;
    virtual Ret move(const io::path_t& src, const io::path_t& dst, bool replace = false) = 0;

    virtual Ret makePath(const io::path_t& path) const = 0;

    virtual EntryType entryType(const io::path_t& path) const = 0;

    virtual RetVal<uint64_t> fileSize(const io::path_t& path) const = 0;

    virtual RetVal<io::paths_t> scanFiles(const io::path_t& rootDir, const std::vector<std::string>& filters,
                                          ScanMode mode = ScanMode::FilesInCurrentDirAndSubdirs) const = 0;

    enum class Attribute {
        Hidden
    };

    virtual void setAttribute(const io::path_t& path, Attribute attribute) const = 0;
    virtual bool setPermissionsAllowedForAll(const io::path_t& path) const = 0;

    virtual RetVal<ByteArray> readFile(const io::path_t& filePath) const = 0;
    virtual Ret readFile(const io::path_t& filePath, ByteArray& data) const = 0;
    virtual Ret writeFile(const io::path_t& filePath, const ByteArray& data) const = 0;

    //! NOTE File info
    virtual io::path_t canonicalFilePath(const io::path_t& filePath) const = 0;
    virtual io::path_t absolutePath(const io::path_t& filePath) const = 0;
    virtual io::path_t absoluteFilePath(const io::path_t& filePath) const = 0;
    virtual DateTime birthTime(const io::path_t& filePath) const = 0;
    virtual DateTime lastModified(const io::path_t& filePath) const = 0;
    virtual Ret isWritable(const io::path_t& filePath) const = 0;
};
}

#endif // MU_IO_IFILESYSTEM_H
