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

#include "dockwindowactionscontroller.h"

#include "../idockwindow.h"

using namespace mu::dock;
using namespace mu::actions;

static QString dockNameFromArgs(const ActionData& args)
{
    return !args.empty() ? args.arg<QString>(0) : QString();
}

void DockWindowActionsController::init()
{
    dispatcher()->reg(this, "dock-set-open", this, &DockWindowActionsController::setDockOpen);
    dispatcher()->reg(this, "dock-toggle", this, &DockWindowActionsController::toggleOpened);
    dispatcher()->reg(this, "dock-toggle-floating", this, &DockWindowActionsController::toggleFloating);
    dispatcher()->reg(this, "dock-restore-default-layout", this, &DockWindowActionsController::restoreDefaultLayout);
}

void DockWindowActionsController::setDockOpen(const ActionData& args)
{
    if (args.count() < 2) {
        return;
    }

    QString dockName = args.arg<QString>(0);
    bool open = args.arg<bool>(1);

    window()->setDockOpen(dockName, open);
}

void DockWindowActionsController::toggleOpened(const ActionData& args)
{
    QString dockName = dockNameFromArgs(args);
    window()->toggleDock(dockName);
}

void DockWindowActionsController::toggleFloating(const ActionData& args)
{
    QString dockName = dockNameFromArgs(args);
    window()->toggleDockFloating(dockName);
}

IDockWindow* DockWindowActionsController::window() const
{
    return dockWindowProvider()->window();
}

void DockWindowActionsController::restoreDefaultLayout()
{
    window()->restoreDefaultLayout();
}
