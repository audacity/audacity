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
#include "keynavigationcontrol.h"

using namespace mu::ui;

KeyNavigationControl::KeyNavigationControl(QObject* parent)
    : AbstractKeyNavigation(parent)
{
}

KeyNavigationControl::~KeyNavigationControl()
{
    if (m_subsection) {
        m_subsection->removeControl(this);
    }
}

QString KeyNavigationControl::name() const
{
    return AbstractKeyNavigation::name();
}

const IKeyNavigation::Index& KeyNavigationControl::index() const
{
    return AbstractKeyNavigation::index();
}

mu::async::Channel<IKeyNavigation::Index> KeyNavigationControl::indexChanged() const
{
    return AbstractKeyNavigation::indexChanged();
}

bool KeyNavigationControl::enabled() const
{
    return AbstractKeyNavigation::enabled();
}

mu::async::Channel<bool> KeyNavigationControl::enabledChanged() const
{
    return AbstractKeyNavigation::enabledChanged();
}

bool KeyNavigationControl::active() const
{
    return AbstractKeyNavigation::active();
}

void KeyNavigationControl::setActive(bool arg)
{
    AbstractKeyNavigation::setActive(arg);
}

mu::async::Channel<bool> KeyNavigationControl::activeChanged() const
{
    return AbstractKeyNavigation::activeChanged();
}

void KeyNavigationControl::onEvent(EventPtr e)
{
    AbstractKeyNavigation::onEvent(e);
}

void KeyNavigationControl::trigger()
{
    emit triggered();
}

mu::async::Channel<IKeyNavigationControl*> KeyNavigationControl::forceActiveRequested() const
{
    return m_forceActiveRequested;
}

void KeyNavigationControl::forceActive()
{
    m_forceActiveRequested.send(this);
}

void KeyNavigationControl::setSubSection(KeyNavigationSubSection* subsection)
{
    if (m_subsection == subsection) {
        return;
    }

    if (m_subsection) {
        m_subsection->removeControl(this);
        m_subsection->disconnect(this);
    }

    m_subsection = subsection;

    if (m_subsection) {
        m_subsection->addControl(this);
        connect(m_subsection, &KeyNavigationSubSection::destroyed, this, &KeyNavigationControl::onSubSectionDestroyed);
    }

    emit subsectionChanged(m_subsection);
}

void KeyNavigationControl::onSubSectionDestroyed()
{
    m_subsection = nullptr;
}

KeyNavigationSubSection* KeyNavigationControl::subsection() const
{
    return m_subsection;
}
