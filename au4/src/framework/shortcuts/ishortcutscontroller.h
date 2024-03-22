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
#ifndef MU_SHORTCUTS_ISHORTCUTSCONTROLLER_H
#define MU_SHORTCUTS_ISHORTCUTSCONTROLLER_H

#include <string>

#include "modularity/imoduleinterface.h"
#include "shortcutstypes.h"

namespace mu::shortcuts {
class IShortcutsController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IShortcutsController)

public:
    virtual ~IShortcutsController() = default;

    virtual void activate(const std::string& sequence) = 0;
    virtual bool isRegistered(const std::string& sequence) const = 0;
};
}

#endif // MU_SHORTCUTS_ISHORTCUTSCONTROLLER_H
