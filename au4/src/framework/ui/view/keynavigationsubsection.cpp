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
#include "keynavigationsubsection.h"

#include <algorithm>

#include "log.h"

#include "keynavigationcontrol.h"

using namespace mu::ui;

KeyNavigationSubSection::KeyNavigationSubSection(QObject* parent)
    : AbstractKeyNavigation(parent)
{
}

KeyNavigationSubSection::~KeyNavigationSubSection()
{
    if (m_section) {
        m_section->removeSubSection(this);
    }
}

QString KeyNavigationSubSection::name() const
{
    return AbstractKeyNavigation::name();
}

const IKeyNavigation::Index& KeyNavigationSubSection::index() const
{
    return AbstractKeyNavigation::index();
}

mu::async::Channel<IKeyNavigation::Index> KeyNavigationSubSection::indexChanged() const
{
    return AbstractKeyNavigation::indexChanged();
}

bool KeyNavigationSubSection::enabled() const
{
    return AbstractKeyNavigation::enabled();
}

mu::async::Channel<bool> KeyNavigationSubSection::enabledChanged() const
{
    return AbstractKeyNavigation::enabledChanged();
}

bool KeyNavigationSubSection::active() const
{
    return AbstractKeyNavigation::active();
}

void KeyNavigationSubSection::setActive(bool arg)
{
    AbstractKeyNavigation::setActive(arg);
}

mu::async::Channel<bool> KeyNavigationSubSection::activeChanged() const
{
    return AbstractKeyNavigation::activeChanged();
}

void KeyNavigationSubSection::onEvent(EventPtr e)
{
    AbstractKeyNavigation::onEvent(e);
}

void KeyNavigationSubSection::setDirection(QmlDirection direction)
{
    if (m_direction == direction) {
        return;
    }

    m_direction = direction;
    emit directionChanged(m_direction);
}

KeyNavigationSubSection::QmlDirection KeyNavigationSubSection::direction_property() const
{
    return m_direction;
}

IKeyNavigationSubSection::Direction KeyNavigationSubSection::direction() const
{
    return static_cast<Direction>(m_direction);
}

const std::set<IKeyNavigationControl*>& KeyNavigationSubSection::controls() const
{
    return m_controls;
}

mu::async::Notification KeyNavigationSubSection::controlsListChanged() const
{
    return m_controlsListChanged;
}

mu::async::Channel<SubSectionControl> KeyNavigationSubSection::forceActiveRequested() const
{
    return m_forceActiveRequested;
}

KeyNavigationSection* KeyNavigationSubSection::section() const
{
    return m_section;
}

void KeyNavigationSubSection::setSection(KeyNavigationSection* section)
{
    if (m_section == section) {
        return;
    }

    if (m_section) {
        m_section->removeSubSection(this);
        m_section->disconnect(this);
    }

    m_section = section;

    if (m_section) {
        m_section->addSubSection(this);
        connect(m_section, &KeyNavigationSection::destroyed, this, &KeyNavigationSubSection::onSectionDestroyed);
    }

    emit sectionChanged(m_section);
}

void KeyNavigationSubSection::onSectionDestroyed()
{
    m_section = nullptr;
}

void KeyNavigationSubSection::componentComplete()
{
}

void KeyNavigationSubSection::addControl(KeyNavigationControl* control)
{
    IF_ASSERT_FAILED(control) {
        return;
    }

    m_controls.insert(control);

    control->forceActiveRequested().onReceive(this, [this](IKeyNavigationControl* c) {
        m_forceActiveRequested.send(std::make_tuple(this, c));
    });

    if (m_controlsListChanged.isConnected()) {
        m_controlsListChanged.notify();
    }
}

void KeyNavigationSubSection::removeControl(KeyNavigationControl* control)
{
    IF_ASSERT_FAILED(control) {
        return;
    }

    m_controls.erase(control);
    control->forceActiveRequested().resetOnReceive(this);

    if (m_controlsListChanged.isConnected()) {
        m_controlsListChanged.notify();
    }
}
