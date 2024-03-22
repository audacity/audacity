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

#include "id.h"

using namespace mu;

ID::ID()
    : m_id(0)
{
}

ID::ID(const ID& id)
    : m_id(id.m_id)
{
}

ID::ID(const std::string& id)
    : m_id(!id.empty() ? std::stoul(id) : 0)
{
}

bool ID::isValid() const
{
    return m_id != 0;
}

ID& ID::operator=(const ID& id)
{
    m_id = id.m_id;
    return *this;
}

ID& ID::operator+=(const ID& id)
{
    m_id += id.m_id;
    return *this;
}

ID ID::operator+(const ID& id) const
{
    return ID(m_id + id.m_id);
}

ID ID::operator^(const ID& id) const
{
    return ID(m_id ^ id.m_id);
}

bool ID::operator==(const ID& id) const
{
    return m_id == id.m_id;
}

bool ID::operator==(uint64_t id) const
{
    return m_id == id;
}

bool ID::operator!=(const ID& id) const
{
    return m_id != id.m_id;
}

bool ID::operator<(const ID& id) const
{
    return m_id < id.m_id;
}

bool ID::operator>(const ID& id) const
{
    return m_id > id.m_id;
}

uint64_t ID::toUint64() const
{
    return m_id;
}

std::string ID::toStdString() const
{
    return std::to_string(m_id);
}

#ifndef NO_QT_SUPPORT
ID::ID(const QString& id)
    : m_id(id.toULongLong())
{
}

ID::ID(const QVariant& id)
    : m_id(id.toULongLong())
{
}

QString ID::toQString() const
{
    return QString::number(m_id);
}

#endif
