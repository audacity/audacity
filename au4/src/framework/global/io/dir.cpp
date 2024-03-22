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
#include "dir.h"

using namespace mu;
using namespace mu::io;

Dir::Dir(const path_t& path)
    : m_path(path)
{
}

path_t Dir::path() const
{
    return m_path;
}

path_t Dir::absolutePath() const
{
    return fileSystem()->absoluteFilePath(m_path);
}

bool Dir::exists() const
{
    return fileSystem()->exists(m_path);
}

Ret Dir::removeRecursively()
{
    return fileSystem()->remove(m_path);
}

Ret Dir::mkpath()
{
    return mkpath(m_path);
}

Ret Dir::mkpath(const path_t& path)
{
    return fileSystem()->makePath(path);
}

RetVal<io::paths_t> Dir::scanFiles(const io::path_t& rootDir, const std::vector<std::string>& filters, ScanMode mode)
{
    return fileSystem()->scanFiles(rootDir, filters, mode);
}

path_t Dir::fromNativeSeparators(const path_t& pathName)
{
#if defined(Q_OS_WIN)
    String path = pathName.toString();
    size_t i = path.indexOf(u'\\');
    if (i != mu::nidx) {
        String n(path);
        if (n.startsWith(u"\\\\?\\")) {
            n.remove(0, 4);
            i = n.indexOf(u'\\');
            if (i == mu::nidx) {
                return n;
            }
        }

        for (i = 0; i < n.size(); ++i) {
            if (n.at(i) == u'\\') {
                n[i] = u'/';
            }
        }

        return n;
    }
#endif
    return pathName;
}
