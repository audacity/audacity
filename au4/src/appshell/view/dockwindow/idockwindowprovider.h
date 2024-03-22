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
#ifndef MU_DOCK_IDOCKWINDOWPROVIDER_H
#define MU_DOCK_IDOCKWINDOWPROVIDER_H

#include "modularity/imoduleinterface.h"
#include "async/notification.h"

namespace mu::dock {
class IDockWindow;
class IDockWindowProvider : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IDockWindowProvider)

public:
    virtual ~IDockWindowProvider() = default;

    virtual void init(IDockWindow* window) = 0;
    virtual void deinit() = 0;

    virtual IDockWindow* window() const = 0;
    virtual async::Notification windowChanged() const = 0;
};
}

#endif // MU_DOCK_IDOCKWINDOWPROVIDER_H
