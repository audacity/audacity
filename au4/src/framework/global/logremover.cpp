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
#include "logremover.h"

#include "io/file.h"
#include "io/dir.h"

#include "log.h"

using namespace mu;

void LogRemover::removeLogs(const io::path_t& logsDir, int olderThanDays, const String& pattern)
{
    //! NOTE If the pattern changes,
    //! then we need to change the implementation of `scanDir` and `parseDate` functions.
    IF_ASSERT_FAILED(pattern.endsWith(u"_yyMMdd.log") || pattern.endsWith(u"_yyMMdd_HHmmss.log")) {
        return;
    }

    io::paths_t files;
    scanDir(logsDir, files);

    Date currentDate = Date::currentDate();

    io::paths_t toRemoveFiles;
    for (const io::path_t& file : files) {
        Date date = parseDate(io::filename(file).toString());
        if (date.isNull()) {
            continue;
        }

        int days = date.daysTo(currentDate);
        if (days >= olderThanDays) {
            toRemoveFiles.push_back(file);
        }
    }

    removeFiles(toRemoveFiles);
}

mu::Date LogRemover::parseDate(const String& fileName)
{
    size_t dateStartIdx = fileName.indexOf(u'_');
    if (dateStartIdx == mu::nidx) {
        return Date();
    }

    String dateStr = fileName.mid(dateStartIdx + 1, fileName.size() - 1);

    // "yyMMdd"
    String yy = dateStr.mid(0, 2);
    bool ok = false;
    int y = yy.toInt(&ok);
    if (!ok || !(y > 0)) {
        return Date();
    }

    String mm = dateStr.mid(2, 2);
    int m = mm.toInt(&ok);
    if (!ok || !(m > 0 && m <= 12)) {
        return Date();
    }

    String dd = dateStr.mid(4, 2);
    int d = dd.toInt(&ok);
    if (!ok || !(d > 0 && d <= 31)) {
        return Date();
    }

    Date date(2000 + y, m, d);
    return date;
}

void LogRemover::removeFiles(const io::paths_t& files)
{
    for (const io::path_t& file : files) {
        io::File::remove(file);
    }
}

void LogRemover::scanDir(const io::path_t& logsDir, io::paths_t& files)
{
    RetVal<io::paths_t> rv = io::Dir::scanFiles(logsDir, { "*.log" }, io::ScanMode::FilesInCurrentDir);
    if (!rv.ret) {
        LOGE() << "failed scan dir: " << logsDir;
    }

    files = rv.val;
}
