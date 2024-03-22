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
#ifndef MU_UI_NAVIGATIONSECTIONMOCK_H
#define MU_UI_NAVIGATIONSECTIONMOCK_H

#include <gmock/gmock.h>

#include "framework/ui/inavigation.h"

namespace mu::ui {
class NavigationSectionMock : public INavigationSection
{
public:

    MOCK_METHOD(Type, type, (), (const, override));
    MOCK_METHOD(QString, name, (), (const, override));

    MOCK_METHOD(const Index&, index, (), (const, override));
    MOCK_METHOD(async::Channel<Index>, indexChanged, (), (const, override));

    MOCK_METHOD(bool, enabled, (), (const, override));
    MOCK_METHOD(async::Channel<bool>, enabledChanged, (), (const, override));

    MOCK_METHOD(bool, active, (), (const, override));
    MOCK_METHOD(void, setActive, (bool), (override));
    MOCK_METHOD(async::Channel<bool>, activeChanged, (), (const, override));

    MOCK_METHOD(void, onEvent, (EventPtr), (override));

    MOCK_METHOD(QWindow*, window, (), (const, override));

    MOCK_METHOD(const std::set<INavigationPanel*>&, panels, (), (const, override));
    MOCK_METHOD(async::Notification, panelsListChanged, (), (const, override));

    MOCK_METHOD(void, setOnActiveRequested, (const OnActiveRequested& func), (override));
    MOCK_METHOD(void, requestActive, (INavigationPanel*, INavigationControl*, bool, INavigation::ActivationType), (override));
};

class NavigationPanelMock : public INavigationPanel
{
public:

    MOCK_METHOD(QString, name, (), (const, override));

    MOCK_METHOD(const Index&, index, (), (const, override));
    MOCK_METHOD(async::Channel<Index>, indexChanged, (), (const, override));

    MOCK_METHOD(bool, enabled, (), (const, override));
    MOCK_METHOD(async::Channel<bool>, enabledChanged, (), (const, override));

    MOCK_METHOD(bool, active, (), (const, override));
    MOCK_METHOD(void, setActive, (bool), (override));
    MOCK_METHOD(async::Channel<bool>, activeChanged, (), (const, override));

    MOCK_METHOD(void, onEvent, (EventPtr), (override));

    MOCK_METHOD(QWindow*, window, (), (const, override));

    MOCK_METHOD(INavigationSection*, section, (), (const, override));
    MOCK_METHOD(Direction, direction, (), (const, override));
    MOCK_METHOD(const std::set<INavigationControl*>&, controls, (), (const, override));
    MOCK_METHOD(async::Notification, controlsListChanged, (), (const, override));

    MOCK_METHOD(void, requestActive, (INavigationControl*, bool, INavigation::ActivationType), (override));
};

class NavigationControlMock : public INavigationControl
{
public:

    MOCK_METHOD(QString, name, (), (const, override));

    MOCK_METHOD(const Index&, index, (), (const, override));
    MOCK_METHOD(async::Channel<Index>, indexChanged, (), (const, override));

    MOCK_METHOD(bool, enabled, (), (const, override));
    MOCK_METHOD(async::Channel<bool>, enabledChanged, (), (const, override));

    MOCK_METHOD(bool, active, (), (const, override));
    MOCK_METHOD(void, setActive, (bool), (override));
    MOCK_METHOD(async::Channel<bool>, activeChanged, (), (const, override));

    MOCK_METHOD(void, onEvent, (EventPtr), (override));

    MOCK_METHOD(QWindow*, window, (), (const, override));

    MOCK_METHOD(INavigationPanel*, panel, (), (const, override));

    MOCK_METHOD(void, trigger, (), (override));
    MOCK_METHOD(void, requestActive, (bool), (override));
};
}

#endif // MU_UI_NAVIGATIONSECTIONMOCK_H
