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
#include "fileinfo.h"

using namespace mu;
using namespace mu::io;

FileInfo::FileInfo(const path_t& filePath)
    : m_filePath(filePath.toString())
{
}

String FileInfo::path() const
{
    size_t lastSep = m_filePath.lastIndexOf(u'/');
    if (lastSep == mu::nidx) {
#if defined(Q_OS_WIN)
        if (m_filePath.size() >= 2 && m_filePath.at(1) == u':') {
            return m_filePath.left(2);
        }
#endif
        return String(u'.');
    }
    if (lastSep == 0) {
        return String(u'/');
    }
#if defined(Q_OS_WIN)
    if (lastSep == 2 && m_filePath.at(1) == u':') {
        return m_filePath.left(lastSep + 1);
    }
#endif
    return m_filePath.left(lastSep);
}

String FileInfo::filePath() const
{
    return m_filePath;
}

String FileInfo::canonicalFilePath() const
{
    return fileSystem()->canonicalFilePath(m_filePath).toString();
}

String FileInfo::absolutePath() const
{
    return fileSystem()->absolutePath(m_filePath).toString();
}

String FileInfo::fileName() const
{
    size_t lastSep = m_filePath.lastIndexOf(u'/');
#if defined(Q_OS_WIN)
    if (lastSep == mu::nidx && m_filePath.size() >= 2 && m_filePath.at(1) == u':') {
        return m_filePath.mid(2);
    }
#endif
    return m_filePath.mid(lastSep + 1);
}

String FileInfo::baseName() const
{
    size_t lastSep = m_filePath.lastIndexOf(u'/');
    size_t from = lastSep + 1;
    size_t firstDot = m_filePath.indexOf(u'.', from);
    size_t to = firstDot > 0 ? firstDot : m_filePath.size();
    size_t length = to - from;

#if defined(Q_OS_WIN)
    if (lastSep == mu::nidx && m_filePath.size() >= 2 && m_filePath.at(1) == u':') {
        return m_filePath.mid(2, length - 2);
    }
#endif
    return m_filePath.mid(from, length);
}

String FileInfo::completeBaseName() const
{
    size_t lastSep = m_filePath.lastIndexOf(u'/');
    size_t from = lastSep + 1;
    size_t lastDot = m_filePath.lastIndexOf(u'.');
    size_t to = lastDot > 0 ? lastDot : m_filePath.size();
    size_t length = to - from;

#if defined(Q_OS_WIN)
    if (lastSep == mu::nidx && m_filePath.size() >= 2 && m_filePath.at(1) == u':') {
        return m_filePath.mid(2, length - 2);
    }
#endif
    return m_filePath.mid(from, length);
}

String FileInfo::suffix() const
{
    return doSuffix(m_filePath);
}

String FileInfo::suffix(const path_t& filePath)
{
    return doSuffix(filePath.toString());
}

String FileInfo::doSuffix(const String& filePath)
{
    size_t lastDot = filePath.lastIndexOf(u'.');
    if (lastDot == mu::nidx) {
        return String();
    }

    size_t lastSep = filePath.lastIndexOf(u'/');
    if (lastSep == mu::nidx) {
        lastSep = 0;
    }

    if (lastDot < lastSep) {
        return String();
    }

    return filePath.mid(lastDot + 1);
}

DateTime FileInfo::birthTime() const
{
    return fileSystem()->birthTime(m_filePath);
}

DateTime FileInfo::lastModified() const
{
    return fileSystem()->lastModified(m_filePath);
}

EntryType FileInfo::entryType() const
{
    return fileSystem()->entryType(m_filePath);
}

bool FileInfo::isRelative() const
{
    return !isAbsolute();
}

bool FileInfo::isAbsolute() const
{
#ifdef Q_OS_WIN
    return (m_filePath.size() >= 3
            && m_filePath.at(0).isLetter()
            && m_filePath.at(1) == u':'
            && m_filePath.at(2) == u'/')
           || (!m_filePath.isEmpty()
               && (m_filePath.at(0) == u'/'
                   || m_filePath.at(0) == u':'));
#else
    return !m_filePath.empty() && (m_filePath.at(0) == u'/' || m_filePath.at(0) == u':');
#endif
}

bool FileInfo::exists() const
{
    return fileSystem()->exists(m_filePath);
}

bool FileInfo::exists(const path_t& filePath)
{
    return fileSystem()->exists(filePath);
}

path_t FileInfo::dirPath() const
{
    size_t lastSep = m_filePath.lastIndexOf(u'/');
    if (lastSep == mu::nidx) {
        return ".";
    }
    return m_filePath.mid(0, lastSep);
}

Dir FileInfo::dir() const
{
    return Dir(dirPath());
}
