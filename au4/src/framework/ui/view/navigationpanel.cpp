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
#include "navigationpanel.h"

#include <algorithm>

#include "accessibility/iaccessible.h"
#include "navigationsection.h"
#include "translation.h"
#include "log.h"

using namespace mu::ui;
using namespace mu::accessibility;

NavigationPanel::NavigationPanel(QObject* parent)
    : AbstractNavigation(parent)
{
    accessible()->setRole(MUAccessible::Panel);
}

NavigationPanel::~NavigationPanel()
{
    if (m_section) {
        m_section->removePanel(this);
    }
}

QString NavigationPanel::name() const
{
    return AbstractNavigation::name();
}

const INavigation::Index& NavigationPanel::index() const
{
    return AbstractNavigation::index();
}

void NavigationPanel::setIndex(const Index& index)
{
    AbstractNavigation::setIndex(index);
}

mu::async::Channel<INavigation::Index> NavigationPanel::indexChanged() const
{
    return AbstractNavigation::indexChanged();
}

bool NavigationPanel::enabled() const
{
    if (!AbstractNavigation::enabled()) {
        return false;
    }

    bool enbl = false;
    for (INavigationControl* c : m_controls) {
        if (c->enabled()) {
            enbl = true;
            break;
        }
    }
    return enbl;
}

mu::async::Channel<bool> NavigationPanel::enabledChanged() const
{
    return AbstractNavigation::enabledChanged();
}

bool NavigationPanel::active() const
{
    return AbstractNavigation::active();
}

void NavigationPanel::setActive(bool arg)
{
    AbstractNavigation::setActive(arg);
    if (m_accessible) {
        m_accessible->setState(IAccessible::State::Active, arg);
    }
}

mu::async::Channel<bool> NavigationPanel::activeChanged() const
{
    return AbstractNavigation::activeChanged();
}

void NavigationPanel::onEvent(EventPtr e)
{
    AbstractNavigation::onEvent(e);
}

QWindow* NavigationPanel::window() const
{
    return AbstractNavigation::window();
}

void NavigationPanel::setDirection(QmlDirection direction)
{
    if (m_direction == direction) {
        return;
    }

    m_direction = direction;
    emit directionChanged();
}

NavigationPanel::QmlDirection NavigationPanel::direction_property() const
{
    return m_direction;
}

QString NavigationPanel::directionInfo() const
{
    switch (m_direction) {
    case Horizontal: return qtrc("ui", "direction is horizontal");
    case Vertical: return qtrc("ui", "direction is vertical");
    case Both: return qtrc("ui", "direction is both");
    }
    return QString();
}

INavigationPanel::Direction NavigationPanel::direction() const
{
    return static_cast<Direction>(m_direction);
}

const std::set<INavigationControl*>& NavigationPanel::controls() const
{
    return m_controls;
}

mu::async::Notification NavigationPanel::controlsListChanged() const
{
    return m_controlsListChanged;
}

INavigationSection* NavigationPanel::section() const
{
    return m_section;
}

NavigationSection* NavigationPanel::section_property() const
{
    return m_section;
}

void NavigationPanel::setSection(INavigationSection* section)
{
    setSection_property(dynamic_cast<NavigationSection*>(section));
}

void NavigationPanel::setSection_property(NavigationSection* section)
{
    TRACEFUNC;
    if (m_section == section) {
        return;
    }

    if (m_section) {
        m_section->removePanel(this);
        m_section->disconnect(this);
    }

    m_section = section;

    if (m_section) {
        m_section->addPanel(this);
        connect(m_section, &NavigationSection::destroyed, this, &NavigationPanel::onSectionDestroyed);
    }

    emit sectionChanged(m_section);
}

void NavigationPanel::onSectionDestroyed()
{
    m_section = nullptr;
}

void NavigationPanel::addControl(NavigationControl* control)
{
    TRACEFUNC;
    IF_ASSERT_FAILED(control) {
        return;
    }

    m_controls.insert(control);

    if (m_controlsListChanged.isConnected()) {
        m_controlsListChanged.notify();
    }
}

void NavigationPanel::removeControl(NavigationControl* control)
{
    TRACEFUNC;
    IF_ASSERT_FAILED(control) {
        return;
    }

    m_controls.erase(control);

    if (m_controlsListChanged.isConnected()) {
        m_controlsListChanged.notify();
    }
}

void NavigationPanel::requestActive(INavigationControl* control, bool enableHighlight,
                                    INavigation::ActivationType activationType)
{
    if (m_section) {
        m_section->requestActive(this, control, enableHighlight, activationType);
    }
}
