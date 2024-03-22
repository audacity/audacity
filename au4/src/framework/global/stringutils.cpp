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
#include "stringutils.h"

#include <cctype>
#include <algorithm>

bool mu::strings::replace(std::string& str, const std::string& from, const std::string& to)
{
    size_t start_pos = str.find(from);
    if (start_pos == std::string::npos) {
        return false;
    }
    str.replace(start_pos, from.length(), to);
    return true;
}

void mu::strings::split(const std::string& str, std::vector<std::string>& out, const std::string& delim)
{
    std::size_t current, previous = 0;
    current = str.find(delim);
    std::size_t delimLen = delim.length();

    while (current != std::string::npos) {
        out.push_back(str.substr(previous, current - previous));
        previous = current + delimLen;
        current = str.find(delim, previous);
    }
    out.push_back(str.substr(previous, current - previous));
}

std::string mu::strings::join(const std::vector<std::string>& strs, const std::string& sep)
{
    std::string str;
    bool first = true;
    for (const std::string& s : strs) {
        if (!first) {
            str += sep;
        }
        first = false;
        str += s;
    }
    return str;
}

void mu::strings::ltrim(std::string& s)
{
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char ch) {
        return !std::isspace(ch);
    }));
}

void mu::strings::rtrim(std::string& s)
{
    s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
        return !std::isspace(ch);
    }).base(), s.end());
}

void mu::strings::trim(std::string& s)
{
    ltrim(s);
    rtrim(s);
}

std::string mu::strings::toLower(const std::string& source)
{
    std::string str = source;
    std::for_each(str.begin(), str.end(), [](char& c) {
        c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
    });
    return str;
}

bool mu::strings::startsWith(const std::string& str, const std::string& start)
{
    if (str.size() < start.size()) {
        return false;
    }

    for (size_t i = 0; i < start.size(); ++i) {
        if (str.at(i) != start.at(i)) {
            return false;
        }
    }

    return true;
}

bool mu::strings::endsWith(const std::string& str, const std::string& ending)
{
    if (ending.size() > str.size()) {
        return false;
    }
    std::string ss = str.substr(str.size() - ending.size());
    return ss.compare(ending.c_str()) == 0;
}

std::string mu::strings::leftJustified(const std::string& val, size_t width)
{
    std::string str;
    str.resize(width, ' ');
    size_t length = width < val.size() ? width : val.size();
    for (size_t i = 0; i < length; ++i) {
        str[i] = val[i];
    }
    return str;
}

bool mu::strings::lessThanCaseInsensitive(const std::string& lhs, const std::string& rhs)
{
    int cmp = toLower(lhs).compare(toLower(rhs));
    if (cmp == 0) {
        return lhs < rhs;
    }

    return cmp < 0;
}

bool mu::strings::lessThanCaseInsensitive(const String& lhs, const String& rhs)
{
    String lhsLower = lhs.toLower(), rhsLower = rhs.toLower();
    if (lhsLower == rhsLower) {
        return lhs < rhs;
    }

    return lhsLower < rhsLower;
}
