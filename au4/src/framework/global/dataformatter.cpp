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
#include "dataformatter.h"

#include "translation.h"

using namespace mu;

double DataFormatter::roundDouble(const double& val, const int decimals)
{
    return String::number(val, decimals).toDouble();
}

String DataFormatter::formatReal(double val, int prec)
{
    return String::number(val, prec);
}

String DataFormatter::formatTimeSince(const Date& date)
{
    Date currentDate = DateTime::currentDateTime().date();
    int days = date.daysTo(currentDate);

    if (days == 0) {
        return mtrc("global", "Today");
    }

    if (days == 1) {
        return mtrc("global", "Yesterday");
    }

    if (days < 7) {
        return mtrc("global", "%n day(s) ago", nullptr, days);
    }

    int weeks = days / 7;

    if (weeks == 1) {
        return mtrc("global", "Last week");
    }

    if (weeks <= 4) {
        return mtrc("global", "%n week(s) ago", nullptr, weeks);
    }

    constexpr int monthsInYear = 12;

    int months = (currentDate.year() - date.year()) * monthsInYear + (currentDate.month() - date.month());

    if (months == 1) {
        return mtrc("global", "Last month");
    }

    if (months < monthsInYear) {
        return mtrc("global", "%n month(s) ago", nullptr, months);
    }

    int years = currentDate.year() - date.year();

    return mtrc("global", "%n year(s) ago", nullptr, years);
}

String DataFormatter::formatFileSize(size_t size)
{
    if (size >= 1024 * 1024 * 1024) {
        double gb = double(size) / (1024 * 1024 * 1024);
        //: Abbreviation of "gigabyte", used to indicate file size
        return mtrc("global", "%1 GB", "gigabyte").arg(formatReal(gb, 2));
    }

    if (size >= 1024 * 1024) {
        double mb = double(size) / (1024 * 1024);
        //: Abbreviation of "megabyte", used to indicate file size
        return mtrc("global", "%1 MB", "megabyte").arg(formatReal(mb, 1));
    }

    if (size >= 1024) {
        double kb = double(size) / 1024;
        //: Abbreviation of "kilobyte", used to indicate file size
        return mtrc("global", "%1 KB", "kilobyte").arg(formatReal(kb, 0));
    }

    //: Used to indicate file size. Ideally, keep the translation short; feel free to use an abbreviation.
    return mtrc("global", "%n byte(s)", nullptr, int(size));
}
