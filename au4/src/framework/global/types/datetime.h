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
#ifndef MU_GLOBAL_DATETIME_H
#define MU_GLOBAL_DATETIME_H

#ifndef NO_QT_SUPPORT
#include <QDateTime>
#endif

#include "string.h"

namespace mu {
enum class DateFormat {
    ISODate,                // ISO 8601
    LocaleShortFormat
};

class Date
{
public:
    Date() = default;
    Date(int y, int m, int d);

    bool isNull() const;

    int year() const;
    int month() const;
    int day() const;

    inline bool operator ==(const Date& d) const { return m_year == d.m_year && m_month == d.m_month && m_day == d.m_day; }
    inline bool operator !=(const Date& d) const { return !this->operator ==(d); }

    int64_t daysTo(const Date& d) const;

    static Date currentDate();

    String toString(DateFormat format = DateFormat::ISODate) const;
    static Date fromStringISOFormat(const String& str);

#ifndef NO_QT_SUPPORT
    QDate toQDate() const { return QDate(m_year, m_month, m_day); }
    static Date fromQDate(const QDate& d) { return Date(d.year(), d.month(), d.day()); }
#endif

private:
    int m_year = 0;
    int m_month = 0;
    int m_day = 0;
};

class Time
{
public:

    Time() = default;
    Time(int h, int m, int s);

    int hour() const;
    int minute() const;
    int second() const;

    inline bool operator ==(const Time& t) const { return m_hour == t.m_hour && m_minute == t.m_minute && m_second == t.m_second; }
    inline bool operator !=(const Time& t) const { return !this->operator ==(t); }

    static Time currentTime();

    String toString(DateFormat format = DateFormat::ISODate) const;
    static Time fromStringISOFormat(const String& str);

#ifndef NO_QT_SUPPORT
    QTime toQTime() const { return QTime(m_hour, m_minute, m_second); }
    static Time fromQTime(const QTime& t) { return Time(t.hour(), t.minute(), t.second()); }
#endif

private:
    int m_hour = 0;
    int m_minute = 0;
    int m_second = 0;
};

class DateTime
{
public:
    DateTime() = default;
    DateTime(const Date& d, const Time& t);

    const Date& date() const;
    const Time& time() const;

    inline bool operator ==(const DateTime& dt) const { return dt.date() == date() && dt.time() == time(); }
    inline bool operator !=(const DateTime& dt) const { return !this->operator ==(dt); }

    static DateTime currentDateTime();

    String toString(DateFormat format = DateFormat::ISODate) const;
    static DateTime fromStringISOFormat(const String& str);

#ifndef NO_QT_SUPPORT
    QDateTime toQDateTime() const { return QDateTime(m_date.toQDate(), m_time.toQTime()); }
    static DateTime fromQDateTime(const QDateTime& dt) { return DateTime(Date::fromQDate(dt.date()), Time::fromQTime(dt.time())); }
#endif

private:

    Date m_date;
    Time m_time;
};
}

#endif // MU_GLOBAL_DATETIME_H
