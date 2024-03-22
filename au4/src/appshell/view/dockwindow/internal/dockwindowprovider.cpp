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

#include "dockwindowprovider.h"

#include "log.h"

using namespace mu::dock;
using namespace mu::async;

void DockWindowProvider::init(IDockWindow* window)
{
    IF_ASSERT_FAILED_X(m_window == nullptr, "Window for this DockWindowProvider is already set. Refusing to set it again") {
        return;
    }

    setWindow(window);
}

void DockWindowProvider::deinit()
{
    setWindow(nullptr);
}

void DockWindowProvider::setWindow(IDockWindow* window)
{
    m_window = window;
    m_windowChanged.notify();
}

IDockWindow* DockWindowProvider::window() const
{
    return m_window;
}

Notification DockWindowProvider::windowChanged() const
{
    return m_windowChanged;
}
