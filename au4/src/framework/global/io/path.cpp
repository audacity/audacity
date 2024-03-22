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
#include "path.h"

#ifndef NO_QT_SUPPORT
#include <QDir>
#endif

#include "stringutils.h"
#include "fileinfo.h"

using namespace mu;
using namespace mu::io;

path_t::path_t(const String& s)
    : m_path(s.toStdString())
{
}

path_t::path_t(const std::string& s)
    : m_path(s)
{
}

path_t::path_t(const char* s)
    : m_path(s ? s : "")
{
}

bool path_t::empty() const
{
    return m_path.empty();
}

size_t path_t::size() const
{
    return m_path.size();
}

bool path_t::withSuffix(const char* str) const
{
    return io::FileInfo::suffix(m_path).toLower() == str;
}

path_t path_t::appendingComponent(const path_t& other) const
{
    if ((!m_path.empty() && m_path.back() == '/')
        || (!other.m_path.empty() && other.m_path.front() == '/')) {
        return m_path + other.m_path;
    }

    return m_path + '/' + other.m_path;
}

path_t path_t::appendingSuffix(const path_t& suffix) const
{
    if (!suffix.m_path.empty() && suffix.m_path.front() == '.') {
        return m_path + suffix.m_path;
    }

    return m_path + '.' + suffix.m_path;
}

String path_t::toString() const
{
    return String::fromStdString(m_path);
}

std::string path_t::toStdString() const
{
    return m_path;
}

const char* path_t::c_str() const
{
    return m_path.c_str();
}

#ifndef NO_QT_SUPPORT
path_t::path_t(const QString& s)
    : m_path(s.toStdString())
{
}

path_t::path_t(const QUrl& u)
    : m_path(u.toLocalFile().toStdString())
{
}

QString path_t::toQString() const
{
    return QString::fromStdString(m_path);
}

QUrl path_t::toQUrl() const
{
    return QUrl::fromLocalFile(QString::fromStdString(m_path));
}

std::wstring path_t::toStdWString() const
{
    return QString::fromStdString(m_path).toStdWString();
}

#endif

std::string mu::io::suffix(const mu::io::path_t& path)
{
    return FileInfo::suffix(path).toLower().toStdString();
}

mu::io::path_t mu::io::filename(const mu::io::path_t& path, bool includingExtension)
{
    FileInfo fi(path);
    return includingExtension ? fi.fileName() : fi.completeBaseName();
}

mu::io::path_t mu::io::basename(const mu::io::path_t& path)
{
    FileInfo fi(path);
    return fi.baseName();
}

mu::io::path_t mu::io::completeBasename(const mu::io::path_t& path)
{
    FileInfo fi(path);
    return fi.completeBaseName();
}

mu::io::path_t mu::io::absolutePath(const path_t& path)
{
    return FileInfo(path).absolutePath();
}

mu::io::path_t mu::io::dirpath(const mu::io::path_t& path)
{
    return FileInfo(path).dir().path();
}

mu::io::path_t mu::io::absoluteDirpath(const mu::io::path_t& path)
{
    return FileInfo(path).dir().absolutePath();
}

bool mu::io::isAbsolute(const path_t& path)
{
    return FileInfo(path).isAbsolute();
}

bool mu::io::isAllowedFileName(const path_t& fn_)
{
    std::string fn = basename(fn_).toStdString();
    if (fn.empty()) {
        return false;
    }

    // Windows filenames are not case sensitive.
    fn = String::fromStdString(fn).toUpper().toStdString();

    static const std::string illegal="<>:\"|?*";

    for (const char& c : fn) {
        // Check for control characters
        if (c > 0 && c < 32) {
            return false;
        }

        // Check for illegal characters
        for (const char& ilc : illegal) {
            if (c == ilc) {
                return false;
            }
        }
    }

    // Check for device names in filenames
    static const std::vector<std::string> devices = {
        "CON", "PRN",  "AUX",  "NUL",
        "COM0", "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9",
        "LPT0", "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9"
    };

    for (const std::string& s : devices) {
        if (fn == s) {
            return false;
        }
    }

    // Check for trailing periods or spaces
    if (fn.back() == '.' || fn.back() == ' ') {
        return false;
    }

    // Check for pathnames that are too long
    if (fn.size() > 96) {
        return false;
    }

    // Since we are checking for a filename, it mustn't be a directory
    if (fn.back() == '\\') {
        return false;
    }

    return true;
}

mu::io::path_t mu::io::escapeFileName(const mu::io::path_t& fn_)
{
    //
    // special characters in filenames are a constant source
    // of trouble, this replaces some of them common in german:
    //
    String fn = fn_.toString();
    fn = fn.simplified();
    fn = fn.replace(' ',  '_');
    fn = fn.replace('\n', '_');
    fn = fn.replace(Char(0xe4), u"ae"); // &auml;
    fn = fn.replace(Char(0xf6), u"oe"); // &ouml;
    fn = fn.replace(Char(0xfc), u"ue"); // &uuml;
    fn = fn.replace(Char(0xdf), u"ss"); // &szlig;
    fn = fn.replace(Char(0xc4), u"Ae"); // &Auml;
    fn = fn.replace(Char(0xd6), u"Oe"); // &Ouml;
    fn = fn.replace(Char(0xdc), u"Ue"); // &Uuml;
    fn = fn.replace(Char(0x266d), u"b"); // musical flat sign, happen in instrument names, so can happen in part (file) names
    fn = fn.replace(Char(0x266f), u"#"); // musical sharp sign, can happen in titles, so can happen in score (file) names
    //FAT/NTFS special chars
    fn = fn.replace('<', '_')
         .replace('>', '_')
         .replace(':', '_')
         .replace('"', '_')
         .replace('/', '_')
         .replace('\\', '_')
         .replace('|', '_')
         .replace('?', '_')
         .replace('*', '_');

    return fn;
}

paths_t mu::io::pathsFromString(const std::string& str, const std::string& delim)
{
    if (str.empty()) {
        return {};
    }

    std::vector<std::string> strs;
    strings::split(str, strs, delim);

    paths_t ps;
    ps.reserve(strs.size());
    for (const std::string& s : strs) {
        ps.push_back(path_t(s));
    }
    return ps;
}

std::string mu::io::pathsToString(const paths_t& ps, const std::string& delim)
{
    std::string result;
    bool first = true;
    for (const path_t& _path: ps) {
        if (!first) {
            result += delim;
        }
        first = false;
        result += _path.toStdString();
    }

    return result;
}

path_t mu::io::toNativeSeparators(const path_t& path)
{
#ifndef NO_QT_SUPPORT
    return QDir::toNativeSeparators(path.toQString());
#else
    return path;
#endif
}
