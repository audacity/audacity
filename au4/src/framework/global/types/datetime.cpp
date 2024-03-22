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
#include "datetime.h"

#include <chrono>
#include <iomanip>
#include <cassert>

using namespace mu;
// ==================================
// Format
// ==================================
static Date dateFromTM(const std::tm& tm)
{
    return Date(tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday);
}

static void toTM(std::tm& tm, const Date& d)
{
    tm.tm_year = std::max(d.year() - 1900, 0);
    tm.tm_mon = std::max(d.month() - 1, 0);
    tm.tm_mday = std::max(d.day(), 1);
}

static Time timeFromTM(const std::tm& tm)
{
    return Time(tm.tm_hour, tm.tm_min, tm.tm_sec);
}

static void toTM(std::tm& tm, const Time& t)
{
    tm.tm_hour = std::max(t.hour(), 0);
    tm.tm_min = std::max(t.minute(), 0);
    tm.tm_sec = std::max(t.second(), 0);
}

static DateTime datetimeFromTM(const std::tm& tm)
{
    return DateTime(dateFromTM(tm), timeFromTM(tm));
}

static void toTM(std::tm& tm, const DateTime& dt)
{
    toTM(tm, dt.date());
    toTM(tm, dt.time());
}

static String formatTime(const Time& t, const char* fmt)
{
    std::tm tm;
    toTM(tm, t);

    char mbstr[100];
    std::strftime(mbstr, sizeof(mbstr), fmt, &tm);

    return String::fromUtf8(mbstr);
}

static String formatDate(const Date& d, const char* fmt)
{
    std::tm tm;
    toTM(tm, d);

    char mbstr[100];
    std::strftime(mbstr, sizeof(mbstr), fmt, &tm);

    return String::fromUtf8(mbstr);
}

static String formatDateTime(const DateTime& dt, const char* fmt)
{
    std::tm tm;
    toTM(tm, dt);

    char mbstr[100];
    std::strftime(mbstr, sizeof(mbstr), fmt, &tm);

    return String::fromUtf8(mbstr);
}

// ==================================
// Date
// ==================================
Date::Date(int y, int m, int d)
    : m_year(y), m_month(m), m_day(d)
{
}

bool Date::isNull() const
{
    return m_year == 0 && m_month == 0 && m_day == 0;
}

int Date::year() const
{
    return m_year;
}

int Date::month() const
{
    return m_month;
}

int Date::day() const
{
    return m_day;
}

int64_t Date::daysTo(const Date& d) const
{
    if (isNull() || d.isNull()) {
        return 0;
    }

    std::tm meTM = {};
    toTM(meTM, *this);

    std::tm otherTM = {};
    toTM(otherTM, d);

    std::time_t meTime = std::mktime(&meTM);
    std::time_t otherTime = std::mktime(&otherTM);
    if (meTime != (std::time_t)(-1) && otherTime != (std::time_t)(-1)) {
        double difference = std::difftime(otherTime, meTime) / (60 * 60 * 24);
        return difference;
    }
    return 0;
}

Date Date::currentDate()
{
    using namespace std::chrono;
    milliseconds ms_d = duration_cast< milliseconds >(system_clock::now().time_since_epoch());

    std::time_t sec = static_cast<std::time_t>(ms_d.count() / 1000);
    std::tm tm;
#ifdef WIN32
    bool err = localtime_s(&tm, &sec) != 0;
#else
    bool err = localtime_r(&sec, &tm) == nullptr;
#endif
    assert(!err);
    if (err) {
        return Date();
    }
    return dateFromTM(tm);
}

String Date::toString(DateFormat format) const
{
    switch (format) {
    case DateFormat::ISODate:           return formatDate(*this, "%Y-%m-%d");
    case DateFormat::LocaleShortFormat: return formatDate(*this, "%x");
    }
    return String();
}

Date Date::fromStringISOFormat(const String& str)
{
    std::tm tm = {};
    std::stringstream ss(str.toStdString());
    ss >> std::get_time(&tm, "%Y-%m-%d");
    return dateFromTM(tm);
}

// ==================================
// Time
// ==================================

Time::Time(int h, int m, int s)
    : m_hour(h), m_minute(m), m_second(s)
{
}

int Time::hour() const
{
    return m_hour;
}

int Time::minute() const
{
    return m_minute;
}

int Time::second() const
{
    return m_second;
}

Time Time::currentTime()
{
    using namespace std::chrono;
    milliseconds ms_d = duration_cast< milliseconds >(system_clock::now().time_since_epoch());

    std::time_t sec = static_cast<std::time_t>(ms_d.count() / 1000);
    std::tm tm;
#ifdef WIN32
    bool err = localtime_s(&tm, &sec) != 0;
#else
    bool err = localtime_r(&sec, &tm) == nullptr;
#endif
    assert(!err);
    if (err) {
        return Time();
    }
    return timeFromTM(tm);
}

String Time::toString(DateFormat format) const
{
    switch (format) {
    case DateFormat::ISODate:           return formatTime(*this, "%H:%M:%S");
    case DateFormat::LocaleShortFormat: return formatTime(*this, "%R");
    }
    return String();
}

Time Time::fromStringISOFormat(const String& str)
{
    std::tm tm = {};
    std::stringstream ss(str.toStdString());
    ss >> std::get_time(&tm, "%H:%M:%S");
    return timeFromTM(tm);
}

// ==================================
// DateTime
// ==================================
DateTime::DateTime(const Date& d, const Time& t)
    : m_date(d), m_time(t)
{
}

const Date& DateTime::date() const
{
    return m_date;
}

const Time& DateTime::time() const
{
    return m_time;
}

DateTime DateTime::currentDateTime()
{
    using namespace std::chrono;
    milliseconds ms_d = duration_cast< milliseconds >(system_clock::now().time_since_epoch());

    std::time_t sec = static_cast<std::time_t>(ms_d.count() / 1000);
    std::tm tm;
#ifdef WIN32
    bool err = localtime_s(&tm, &sec) != 0;
#else
    bool err = localtime_r(&sec, &tm) == nullptr;
#endif
    assert(!err);
    if (err) {
        return DateTime();
    }

    return datetimeFromTM(tm);
}

String DateTime::toString(DateFormat format) const
{
    switch (format) {
    case DateFormat::ISODate:           return formatDateTime(*this, "%Y-%m-%dT%H:%M:%S");
    case DateFormat::LocaleShortFormat: return formatDateTime(*this, "%x %R");
    }
    return String();
}

DateTime DateTime::fromStringISOFormat(const String& str)
{
    std::tm tm = {};
    std::stringstream ss(str.toStdString());
    ss >> std::get_time(&tm, "%Y-%m-%dT%H:%M:%S");
    return datetimeFromTM(tm);
}
