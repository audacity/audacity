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
#include "accessiblestub.h"

#include <QQuickItem>

using namespace mu::accessibility;

AccessibleStub::AccessibleStub(QObject* object)
    : m_object(object)
{
}

QAccessibleInterface* AccessibleStub::accessibleInterface(QObject* object)
{
    if (dynamic_cast<QQuickItem*>(object)) {
        return new AccessibleStub(object);
    }

    return nullptr;
}

bool AccessibleStub::isValid() const
{
    return false;
}

QObject* AccessibleStub::object() const
{
    return m_object;
}

QWindow* AccessibleStub::window() const
{
    return nullptr;
}

QVector<QPair<QAccessibleInterface*, QAccessible::Relation> > AccessibleStub::relations(QAccessible::Relation) const
{
    return {};
}

QAccessibleInterface* AccessibleStub::childAt(int, int) const
{
    return nullptr;
}

QAccessibleInterface* AccessibleStub::parent() const
{
    return nullptr;
}

QAccessibleInterface* AccessibleStub::child(int) const
{
    return nullptr;
}

int AccessibleStub::childCount() const
{
    return 0;
}

int AccessibleStub::indexOfChild(const QAccessibleInterface*) const
{
    return -1;
}

QString AccessibleStub::text(QAccessible::Text) const
{
    return {};
}

void AccessibleStub::setText(QAccessible::Text, const QString&)
{
}

QRect AccessibleStub::rect() const
{
    return {};
}

QAccessible::Role AccessibleStub::role() const
{
    return QAccessible::Role::NoRole;
}

QAccessible::State AccessibleStub::state() const
{
    return {};
}

void* AccessibleStub::interface_cast(QAccessible::InterfaceType)
{
    return nullptr;
}
