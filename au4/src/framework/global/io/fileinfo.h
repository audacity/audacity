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
#ifndef MU_IO_FILEINFO_H
#define MU_IO_FILEINFO_H

#include "global/types/string.h"
#include "global/modularity/ioc.h"
#include "ifilesystem.h"
#include "dir.h"

namespace mu::io {
class FileInfo
{
    static inline Inject<IFileSystem> fileSystem;

public:
    FileInfo() = default;
    FileInfo(const path_t& filePath);

    String path() const;
    String filePath() const;
    String canonicalFilePath() const;
    String absolutePath() const;

    String fileName() const;
    String baseName() const;
    String completeBaseName() const;
    String suffix() const;
    static String suffix(const path_t& filePath);

    EntryType entryType() const;

    bool isRelative() const;
    bool isAbsolute() const;

    bool exists() const;
    static bool exists(const path_t& filePath);

    DateTime birthTime() const;
    DateTime lastModified() const;

    path_t dirPath() const;
    Dir dir() const;

private:
    static String doSuffix(const String& filePath);

    String m_filePath;
};
}

#endif // MU_IO_FILEINFO_H
