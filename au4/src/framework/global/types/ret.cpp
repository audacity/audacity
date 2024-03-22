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

#include "ret.h"

using namespace mu;

Ret::Ret(bool arg)
    : m_code(arg ? int(Code::Ok) : int(Code::UnknownError))
{}

Ret::Ret(int c)
    : m_code(c)
{}

Ret::Ret(Code c)
    : m_code(static_cast<int>(c))
{
}

Ret::Ret(const int& c, const std::string& text)
    : m_code(c), m_text(text)
{}

bool Ret::valid() const
{
    return m_code > int(Code::Undefined);
}

bool Ret::success() const
{
    return m_code == int(Code::Ok);
}

void Ret::setCode(int c)
{
    m_code = c;
}

int Ret::code() const
{
    return m_code;
}

void Ret::setText(const std::string& s)
{
    m_text = s;
}

const std::string& Ret::text() const
{
    return m_text;
}

void Ret::setData(const std::string& key, const std::any& val)
{
    m_data[key] = val;
}

std::any Ret::data(const std::string& key) const
{
    auto it = m_data.find(key);
    if (it != m_data.end()) {
        return it->second;
    }
    return std::any();
}

std::string Ret::toString() const
{
    return "[" + std::to_string(m_code) + "] " + m_text;
}
