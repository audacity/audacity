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
#include "filesystem.h"

#include <QFileInfo>
#include <QDir>
#include <QDirIterator>

#ifdef Q_OS_WIN
#include <windows.h>
#endif

#include "../ioretcodes.h"
#include "log.h"

using namespace mu;
using namespace mu::io;

Ret FileSystem::exists(const io::path_t& path) const
{
    QFileInfo fileInfo(path.toQString());
    if (!fileInfo.exists()) {
        return make_ret(Err::FSNotExist);
    }

    return make_ret(Err::NoError);
}

Ret FileSystem::remove(const io::path_t& path_, bool onlyIfEmpty)
{
    QString path = path_.toQString();
    QFileInfo fileInfo(path);
    if (fileInfo.exists()) {
        return fileInfo.isDir() ? removeDir(path, onlyIfEmpty) : removeFile(path);
    }

    return make_ret(Err::NoError);
}

Ret FileSystem::clear(const io::path_t& path_)
{
    QString path = path_.toQString();
    if (!QFileInfo::exists(path)) {
        return true;
    }

    Ret ret = make_ret(Err::NoError);
    QDirIterator di(path, QDir::AllEntries | QDir::Hidden | QDir::System | QDir::NoDotAndDotDot);
    while (di.hasNext()) {
        di.next();
        const QFileInfo& fi = di.fileInfo();
        const QString& filePath = di.filePath();
        if (fi.isDir() && !fi.isSymLink()) {
            ret = removeDir(filePath); // recursive
        } else {
            bool ok = QFile::remove(filePath);
            if (!ok) { // Read-only files prevent directory deletion on Windows, retry with Write permission.
                const QFile::Permissions permissions = QFile::permissions(filePath);
                if (!(permissions & QFile::WriteUser)) {
                    ok = QFile::setPermissions(filePath, permissions | QFile::WriteUser)
                         && QFile::remove(filePath);
                }
            }

            if (!ok) {
                ret = make_ret(Err::FSRemoveError);
            }
        }

        if (!ret) {
            break;
        }
    }

    return ret;
}

Ret FileSystem::copy(const io::path_t& src, const io::path_t& dst, bool replace)
{
    QFileInfo srcFileInfo(src.toQString());
    if (!srcFileInfo.exists()) {
        return make_ret(Err::FSNotExist);
    }

    QFileInfo dstFileInfo(dst.toQString());
    if (dstFileInfo.exists()) {
        if (!replace) {
            return make_ret(Err::FSAlreadyExists);
        }

        Ret ret = remove(dst);
        if (!ret) {
            return ret;
        }
    }

    Ret ret = copyRecursively(src, dst);
    return ret;
}

Ret FileSystem::move(const io::path_t& src, const io::path_t& dst, bool replace)
{
    QFileInfo srcFileInfo(src.toQString());
    if (!srcFileInfo.exists()) {
        return make_ret(Err::FSNotExist);
    }

    QFileInfo dstFileInfo(dst.toQString());
    if (dstFileInfo.exists()) {
        if (!replace) {
            return make_ret(Err::FSAlreadyExists);
        }

        Ret ret = remove(dst);
        if (!ret) {
            return ret;
        }
    }

    if (srcFileInfo.isDir()) {
        if (!QDir().rename(src.toQString(), dst.toQString())) {
            return make_ret(Err::FSMoveErrors);
        }
    } else {
        if (!QFile::rename(src.toQString(), dst.toQString())) {
            return make_ret(Err::FSMoveErrors);
        }
    }

    return make_ret(Ret::Code::Ok);
}

RetVal<ByteArray> FileSystem::readFile(const io::path_t& filePath) const
{
    RetVal<ByteArray> result;
    Ret ret = exists(filePath);
    if (!ret) {
        result.ret = ret;
        return result;
    }

    QFile file(filePath.toQString());
    if (!file.open(QIODevice::ReadOnly)) {
        result.ret = make_ret(Err::FSReadError);
        return result;
    }

    qint64 size = file.size();
    result.val.resize(static_cast<size_t>(size));

    file.read(reinterpret_cast<char*>(result.val.data()), size);
    file.close();

    result.ret = make_ret(Err::NoError);
    return result;
}

Ret FileSystem::readFile(const io::path_t& filePath, ByteArray& data) const
{
    Ret ret = make_ok();

    QFile file(filePath.toQString());
    if (!file.open(QIODevice::ReadOnly)) {
        ret = make_ret(Err::FSReadError);
        ret.setText(file.errorString().toStdString());
        return ret;
    }

    qint64 size = file.size();
    data.resize(static_cast<size_t>(size));

    if (file.read(reinterpret_cast<char*>(data.data()), size) == -1) {
        ret = make_ret(Err::FSReadError);
        ret.setText(file.errorString().toStdString());
    }

    file.close();

    return ret;
}

Ret FileSystem::writeFile(const io::path_t& filePath, const ByteArray& data) const
{
    Ret ret = make_ok();

    QFile file(filePath.toQString());
    if (!file.open(QIODevice::WriteOnly)) {
        ret = make_ret(Err::FSWriteError);
        ret.setText(file.errorString().toStdString());
        return ret;
    }

    if (file.write(reinterpret_cast<const char*>(data.constData()), static_cast<qint64>(data.size())) == -1) {
        ret = make_ret(Err::FSWriteError);
        ret.setText(file.errorString().toStdString());
    }

    file.close();

    return ret;
}

Ret FileSystem::makePath(const io::path_t& path) const
{
    if (!QDir().mkpath(path.toQString())) {
        return make_ret(Err::FSMakingError);
    }

    return make_ret(Err::NoError);
}

EntryType FileSystem::entryType(const io::path_t& path) const
{
    QFileInfo fi(path.toQString());
    if (fi.isFile()) {
        return EntryType::File;
    } else if (fi.isDir()) {
        return EntryType::Dir;
    }

    return EntryType::Undefined;
}

RetVal<uint64_t> FileSystem::fileSize(const io::path_t& path) const
{
    RetVal<uint64_t> rv;
    rv.ret = exists(path);
    if (!rv.ret) {
        return rv;
    }

    QFileInfo fi(path.toQString());
    rv.val = static_cast<uint64_t>(fi.size());
    return rv;
}

RetVal<io::paths_t> FileSystem::scanFiles(const io::path_t& rootDir, const std::vector<std::string>& nameFilters, ScanMode mode) const
{
    RetVal<io::paths_t> result;
    Ret ret = exists(rootDir);
    if (!ret) {
        result.ret = ret;
        return result;
    }

    QDirIterator::IteratorFlags flags = QDirIterator::NoIteratorFlags;
    QDir::Filters filters = QDir::NoDotAndDotDot | QDir::Readable;

    switch (mode) {
    case ScanMode::FilesInCurrentDir:
        filters |= QDir::Files;
        break;
    case ScanMode::FilesAndFoldersInCurrentDir:
        filters |= QDir::Files | QDir::Dirs;
        break;
    case ScanMode::FilesInCurrentDirAndSubdirs:
        flags |= QDirIterator::Subdirectories;
        filters |= QDir::Files;
        break;
    }

    QStringList qnameFilters;
    for (const std::string& f : nameFilters) {
        qnameFilters << QString::fromStdString(f);
    }

    QDirIterator it(rootDir.toQString(), qnameFilters, filters, flags);

    while (it.hasNext()) {
        result.val.push_back(it.next());
    }

    result.ret = make_ret(Err::NoError);
    return result;
}

Ret FileSystem::removeFile(const io::path_t& path) const
{
    QFile file(path.toQString());
    if (!file.remove()) {
        return make_ret(Err::FSRemoveError);
    }

    return make_ret(Err::NoError);
}

Ret FileSystem::removeDir(const io::path_t& path, bool onlyIfEmpty) const
{
    QDir dir(path.toQString());

    if (onlyIfEmpty && !dir.isEmpty()) {
        return make_ret(Err::FSDirNotEmptyError);
    }

    if (!dir.removeRecursively()) {
        return make_ret(Err::FSRemoveError);
    }

    return make_ret(Err::NoError);
}

Ret FileSystem::copyRecursively(const io::path_t& src, const io::path_t& dst) const
{
    QString srcPath = src.toQString();
    QString dstPath = dst.toQString();

    QFileInfo srcFileInfo(srcPath);
    if (srcFileInfo.isDir()) {
        QDir dstDir(dstPath);
        dstDir.cdUp();
        if (!dstDir.mkdir(QFileInfo(dstPath).fileName())) {
            return make_ret(Err::FSMakingError);
        }
        QDir srcDir(srcPath);
        const QStringList fileNames = srcDir.entryList(QDir::Files | QDir::Dirs | QDir::NoDotAndDotDot | QDir::Hidden | QDir::System);
        for (const QString& fileName : fileNames) {
            const QString newSrcPath = srcPath + QLatin1Char('/') + fileName;
            const QString newDstPath = dstPath + QLatin1Char('/') + fileName;
            Ret ret = copyRecursively(newSrcPath, newDstPath);
            if (!ret) {
                return ret;
            }
        }
    } else {
        if (!QFile::copy(srcPath, dstPath)) {
            return make_ret(Err::FSCopyError);
        }
    }

    return make_ret(Err::NoError);
}

void FileSystem::setAttribute(const io::path_t& path, Attribute attribute) const
{
    switch (attribute) {
    case Attribute::Hidden: {
#ifdef Q_OS_WIN
        const QString nativePath = QDir::toNativeSeparators(path.toQString());
        SetFileAttributes((LPCTSTR)nativePath.unicode(), FILE_ATTRIBUTE_HIDDEN);
#endif
    } break;
    }
    UNUSED(path);
}

bool FileSystem::setPermissionsAllowedForAll(const io::path_t& path) const
{
    return QFile::setPermissions(path.toQString(),
                                 QFile::ReadOwner | QFile::WriteOwner | QFile::ExeOwner
                                 | QFile::ReadUser | QFile::WriteUser | QFile::ExeUser
                                 | QFile::ReadGroup | QFile::WriteGroup | QFile::ExeGroup
                                 | QFile::ReadOther | QFile::WriteOther
                                 | QFile::ExeOther);
}

io::path_t FileSystem::canonicalFilePath(const io::path_t& filePath) const
{
    return QFileInfo(filePath.toQString()).canonicalFilePath();
}

io::path_t FileSystem::absolutePath(const io::path_t& filePath) const
{
    return QFileInfo(filePath.toQString()).absolutePath();
}

path_t FileSystem::absoluteFilePath(const path_t& filePath) const
{
    return QFileInfo(filePath.toQString()).absoluteFilePath();
}

DateTime FileSystem::birthTime(const io::path_t& filePath) const
{
    return DateTime::fromQDateTime(QFileInfo(filePath.toQString()).birthTime());
}

DateTime FileSystem::lastModified(const io::path_t& filePath) const
{
    return DateTime::fromQDateTime(QFileInfo(filePath.toQString()).lastModified());
}

Ret FileSystem::isWritable(const io::path_t& filePath) const
{
    Ret ret = make_ok();

    QFileInfo fileInfo(filePath.toQString());

    if (!fileInfo.exists()) {
        QFile file(filePath.toQString());

        if (!file.open(QFile::WriteOnly)) {
            ret = make_ret(Err::FSWriteError);
            ret.setText(file.errorString().toStdString());
        }

        file.close();
        file.remove();
    } else if (!fileInfo.isWritable()) {
        QFile file(filePath.toQString());
        file.open(QFile::WriteOnly);

        ret = make_ret(Err::FSWriteError);
        ret.setText(file.errorString().toStdString());

        file.close();
    }

    return ret;
}
