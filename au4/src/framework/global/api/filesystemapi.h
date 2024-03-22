/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2023 MuseScore BVBA and others
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
#ifndef MU_API_FILESYSTEMAPI_H
#define MU_API_FILESYSTEMAPI_H

#include <QString>

#include "api/apiobject.h"
#include "jsretval.h"

#include "modularity/ioc.h"
#include "global/io/ifilesystem.h"

namespace mu::api {
class FileSystemApi : public ApiObject
{
    Q_OBJECT

    INJECT(io::IFileSystem, fileSystem)
public:
    FileSystemApi(IApiEngine* e);

    // FileInfo
    Q_INVOKABLE QString fileName(const QString& path) const;
    Q_INVOKABLE QString baseName(const QString& path) const;

    // Action
    Q_INVOKABLE JSRet remove(const QString& path);
    Q_INVOKABLE JSRet clear(const QString& path);
    Q_INVOKABLE JSRet copy(const QString& src, const QString& dst, bool replace = false);

    // Scan
    /*  ScanMode:
        FilesInCurrentDir,
        FilesAndFoldersInCurrentDir,
        FilesInCurrentDirAndSubdirs
    */

    Q_INVOKABLE JSRetVal scanFiles(const QString& rootDir, const QStringList& filters,
                                   const QString& mode = "FilesInCurrentDirAndSubdirs") const;

    // Read / Write
    Q_INVOKABLE JSRet writeTextFile(const QString& filePath, const QString& str) const;
    Q_INVOKABLE JSRetVal readTextFile(const QString& filePath) const;
};
}

#endif // MU_API_FILESYSTEMAPI_H
