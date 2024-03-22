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
#include "filtervalue.h"

using namespace mu::uicomponents;

FilterValue::FilterValue(QObject* parent)
    : QObject(parent)
{
}

QString FilterValue::roleName() const
{
    return m_roleName;
}

QVariant FilterValue::roleValue() const
{
    return m_roleValue;
}

CompareType::Type FilterValue::compareType() const
{
    return m_compareType;
}

bool FilterValue::enabled() const
{
    return m_enabled;
}

void FilterValue::setRoleName(QString roleName)
{
    if (m_roleName == roleName) {
        return;
    }

    m_roleName = roleName;
    emit dataChanged();
}

void FilterValue::setRoleValue(QVariant value)
{
    if (m_roleValue == value) {
        return;
    }

    m_roleValue = value;
    emit dataChanged();
}

void FilterValue::setCompareType(CompareType::Type type)
{
    if (m_compareType == type) {
        return;
    }

    m_compareType = type;
    emit dataChanged();
}

void FilterValue::setEnabled(bool enabled)
{
    if (m_enabled == enabled) {
        return;
    }

    m_enabled = enabled;
    emit dataChanged();
}

bool FilterValue::async() const
{
    return m_async;
}

void FilterValue::setAsync(bool async)
{
    if (m_async == async) {
        return;
    }

    m_async = async;
    emit dataChanged();
}
