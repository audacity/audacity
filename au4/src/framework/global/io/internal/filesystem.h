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
#ifndef MU_IO_FILESYSTEM_H
#define MU_IO_FILESYSTEM_H

#include "../ifilesystem.h"

namespace mu::io {
class FileSystem : public IFileSystem
{
public:

    Ret exists(const io::path_t& path) const override;
    Ret remove(const io::path_t& path, bool onlyIfEmpty = false) override;
    Ret clear(const io::path_t& path) override;
    Ret copy(const io::path_t& src, const io::path_t& dst, bool replace = false) override;
    Ret move(const io::path_t& src, const io::path_t& dst, bool replace = false) override;

    Ret makePath(const io::path_t& path) const override;

    EntryType entryType(const io::path_t& path) const override;

    RetVal<uint64_t> fileSize(const io::path_t& path) const override;

    RetVal<io::paths_t> scanFiles(const io::path_t& rootDir, const std::vector<std::string>& filters,
                                  ScanMode mode = ScanMode::FilesInCurrentDirAndSubdirs) const override;

    RetVal<ByteArray> readFile(const io::path_t& filePath) const override;
    Ret readFile(const io::path_t& filePath, ByteArray& data) const override;
    Ret writeFile(const io::path_t& filePath, const ByteArray& data) const override;

    void setAttribute(const io::path_t& path, Attribute attribute) const override;
    bool setPermissionsAllowedForAll(const io::path_t& path) const override;

    io::path_t canonicalFilePath(const io::path_t& filePath) const override;
    io::path_t absolutePath(const io::path_t& filePath) const override;
    io::path_t absoluteFilePath(const io::path_t& filePath) const override;
    DateTime birthTime(const io::path_t& filePath) const override;
    DateTime lastModified(const io::path_t& filePath) const override;
    Ret isWritable(const path_t& filePath) const override;

private:
    Ret removeFile(const io::path_t& path) const;
    Ret removeDir(const io::path_t& path, bool onlyIfEmpty = false) const;
    Ret copyRecursively(const io::path_t& src, const io::path_t& dst) const;
};
}

#endif // MU_IO_FILESYSTEM_H
