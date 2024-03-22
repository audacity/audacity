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
#include "sortervalue.h"

using namespace mu::uicomponents;

SorterValue::SorterValue(QObject* parent)
    : QObject(parent)
{
}

QString SorterValue::roleName() const
{
    return m_roleName;
}

Qt::SortOrder SorterValue::sortOrder() const
{
    return m_sortOrder;
}

bool SorterValue::enabled() const
{
    return m_enabled;
}

void SorterValue::setRoleName(QString roleName)
{
    if (m_roleName == roleName) {
        return;
    }

    m_roleName = roleName;
    emit dataChanged();
}

void SorterValue::setSortOrder(Qt::SortOrder sortOrder)
{
    if (m_sortOrder == sortOrder) {
        return;
    }

    m_sortOrder = sortOrder;
    emit dataChanged();
}

void SorterValue::setEnabled(bool enabled)
{
    if (m_enabled == enabled) {
        return;
    }

    m_enabled = enabled;

    if (!enabled) {
        m_sortOrder = Qt::AscendingOrder;
    }

    emit dataChanged();
}
