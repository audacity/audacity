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
#include "navigationevent.h"

#include "log.h"

using namespace mu::ui;

NavigationEvent::NavigationEvent(INavigation::EventPtr event)
    : m_event(event)
{
}

void NavigationEvent::setEvent(INavigation::EventPtr event)
{
    m_event = event;
}

NavigationEvent::Type NavigationEvent::type() const
{
    IF_ASSERT_FAILED(m_event) {
        return Undefined;
    }
    return static_cast<Type>(m_event->type);
}

void NavigationEvent::setAccepted(bool accepted)
{
    IF_ASSERT_FAILED(m_event) {
        return;
    }
    m_event->accepted = accepted;
}

bool NavigationEvent::accepted() const
{
    IF_ASSERT_FAILED(m_event) {
        return false;
    }
    return m_event->accepted;
}

QVariant NavigationEvent::data(const QString& key) const
{
    IF_ASSERT_FAILED(m_event) {
        return false;
    }
    return m_event->data.value(key);
}

void NavigationEvent::setData(const QString& key, const QVariant& val)
{
    IF_ASSERT_FAILED(m_event) {
        return;
    }
    m_event->data[key] = val;
}
