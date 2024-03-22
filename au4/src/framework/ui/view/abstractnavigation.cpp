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
#include "abstractnavigation.h"

#include <QQuickWindow>

#include "qmlaccessible.h"

#include "log.h"

using namespace mu::ui;
using namespace mu::accessibility;

AbstractNavigation::AbstractNavigation(QObject* parent)
    : QObject(parent)
{
}

void AbstractNavigation::classBegin()
{
}

void AbstractNavigation::componentComplete()
{
    if (m_accessible) {
        m_accessible->setState(IAccessible::State::Enabled, enabled());
        m_accessible->setState(IAccessible::State::Active, active());
        m_accessible->componentComplete();
    }

    navigationController()->highlightChanged().onNotify(this, [this](){
        emit highlightChanged();
    });
}

void AbstractNavigation::setName(QString name)
{
    if (m_name == name) {
        return;
    }

    m_name = name;
    emit nameChanged(m_name);
}

QString AbstractNavigation::name() const
{
    return m_name;
}

const INavigation::Index& AbstractNavigation::index() const
{
    return m_index;
}

void AbstractNavigation::setIndex(const INavigation::Index& index)
{
    if (m_index == index) {
        return;
    }

    bool _rowChanged = m_index.row != index.row;
    bool _columnChanged = m_index.column != index.column;

    m_index = index;

    if (m_indexChanged.isConnected()) {
        m_indexChanged.send(m_index);
    }

    if (_rowChanged) {
        emit rowChanged(m_index.row);
    }

    if (_columnChanged) {
        emit columnChanged(m_index.column);
    }
}

mu::async::Channel<INavigation::Index> AbstractNavigation::indexChanged() const
{
    return m_indexChanged;
}

void AbstractNavigation::setOrder(int order)
{
    if (m_index.order() == order) {
        return;
    }

    m_index.setOrder(order);
    emit orderChanged(order);

    if (m_indexChanged.isConnected()) {
        m_indexChanged.send(m_index);
    }
}

int AbstractNavigation::order() const
{
    return m_index.order();
}

void AbstractNavigation::setColumn(int column)
{
    if (m_index.column == column) {
        return;
    }

    m_index.column = column;
    emit columnChanged(column);

    if (m_indexChanged.isConnected()) {
        m_indexChanged.send(m_index);
    }
}

int AbstractNavigation::column() const
{
    return m_index.column;
}

void AbstractNavigation::setRow(int row)
{
    if (m_index.row == row) {
        return;
    }

    m_index.row = row;
    emit rowChanged(row);

    if (m_indexChanged.isConnected()) {
        m_indexChanged.send(m_index);
    }
}

int AbstractNavigation::row() const
{
    return m_index.row;
}

void AbstractNavigation::setEnabled(bool enabled)
{
    if (m_enabled == enabled) {
        return;
    }

    m_enabled = enabled;
    emit enabledChanged(m_enabled);

    if (m_enabledChanged.isConnected()) {
        m_enabledChanged.send(m_enabled);
    }

    if (m_accessible) {
        m_accessible->setState(IAccessible::State::Enabled, enabled);
    }
}

bool AbstractNavigation::enabled() const
{
    return m_enabled;
}

mu::async::Channel<bool> AbstractNavigation::enabledChanged() const
{
    return m_enabledChanged;
}

void AbstractNavigation::setActive(bool active)
{
    if (m_active == active) {
        return;
    }

    m_active = active;
    emit activeChanged(m_active);
    emit highlightChanged();

    if (m_activeChanged.isConnected()) {
        m_activeChanged.send(m_active);
    }
}

bool AbstractNavigation::active() const
{
    return m_active;
}

mu::async::Channel<bool> AbstractNavigation::activeChanged() const
{
    return m_activeChanged;
}

void AbstractNavigation::onEvent(INavigation::EventPtr e)
{
    NavigationEvent ev(e);
    emit navigationEvent(QVariant::fromValue(ev));
}

QWindow* AbstractNavigation::window() const
{
    QObject* prn = parent();
    while (prn) {
        QQuickItem* vitem = qobject_cast<QQuickItem*>(prn);
        if (vitem) {
            return vitem->window();
        }

        prn = prn->parent();
    }

    return nullptr;
}

AccessibleItem* AbstractNavigation::accessible() const
{
    if (!m_accessible) {
        AbstractNavigation* self = const_cast<AbstractNavigation*>(this);
        m_accessible = new AccessibleItem(self);
    }
    return m_accessible;
}

void AbstractNavigation::setAccessible(AccessibleItem* accessible)
{
    if (m_accessible == accessible) {
        return;
    }

    if (m_accessible) {
        delete m_accessible;
    }

    m_accessible = accessible;

    if (m_accessible) {
        m_accessible->setParent(this);
        m_accessible->setState(IAccessible::State::Enabled, enabled());
        m_accessible->setState(IAccessible::State::Active, active());
    }

    emit accessibleChanged();
}

void AbstractNavigation::setAccessibleParent(AccessibleItem* p)
{
    if (m_accessible) {
        m_accessible->setAccessibleParent(p);
    }
}

bool AbstractNavigation::highlight() const
{
    return active() && navigationController()->isHighlight();
}
