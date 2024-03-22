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
#include "filesystemapi.h"

#include "io/fileinfo.h"

#include "apiutils.h"

#include "log.h"

using namespace mu;
using namespace mu::api;
using namespace mu::io;

FileSystemApi::FileSystemApi(IApiEngine* e)
    : ApiObject(e)
{
}

QString FileSystemApi::fileName(const QString& path) const
{
    return FileInfo(path).fileName().toQString();
}

QString FileSystemApi::baseName(const QString& path) const
{
    return FileInfo(path).baseName().toQString();
}

JSRet FileSystemApi::remove(const QString& path)
{
    Ret ret = fileSystem()->remove(path);
    return retToJs(ret);
}

JSRet FileSystemApi::clear(const QString& path)
{
    Ret ret = fileSystem()->clear(path);
    return retToJs(ret);
}

JSRet FileSystemApi::copy(const QString& src, const QString& dst, bool replace)
{
    Ret ret = fileSystem()->copy(src, dst, replace);
    return retToJs(ret);
}

JSRetVal FileSystemApi::scanFiles(const QString& rootDir, const QStringList& filters,  const QString& mode) const
{
    auto toIoScanMode = [](const QString& m)
    {
        if (m == "FilesInCurrentDir") {
            return io::ScanMode::FilesInCurrentDir;
        } else if (m == "FilesAndFoldersInCurrentDir") {
            return io::ScanMode::FilesAndFoldersInCurrentDir;
        } else if (m == "FilesInCurrentDirAndSubdirs") {
            return io::ScanMode::FilesInCurrentDirAndSubdirs;
        } else {
            LOGE() << "unknown mode: " << m;
        }

        return io::ScanMode::FilesInCurrentDirAndSubdirs;
    };

    RetVal<io::paths_t> rv = fileSystem()->scanFiles(rootDir, toStdVector(filters), toIoScanMode(mode));
    return retValToJs(rv);
}

JSRet FileSystemApi::writeTextFile(const QString& filePath, const QString& str) const
{
    QByteArray data = str.toUtf8();
    Ret ret = fileSystem()->writeFile(filePath, ByteArray::fromQByteArrayNoCopy(data));
    return retToJs(ret);
}

JSRetVal FileSystemApi::readTextFile(const QString& filePath) const
{
    RetVal<ByteArray> data = fileSystem()->readFile(filePath);
    RetVal<QString> sr;
    sr.ret = data.ret;
    sr.val = QString::fromUtf8(data.val.toQByteArrayNoCopy());
    return retValToJs(sr);
}
