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

#ifndef MU_ACCESSIBILITY_IACCESSIBILITYCONTROLLER_H
#define MU_ACCESSIBILITY_IACCESSIBILITYCONTROLLER_H

#include "modularity/imoduleinterface.h"
#include "iaccessible.h"

namespace mu::accessibility {
class IAccessibilityController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAccessibilityController)
public:
    virtual ~IAccessibilityController() = default;

    virtual void reg(IAccessible* item) = 0;
    virtual void unreg(IAccessible* item) = 0;

    virtual const IAccessible* accessibleRoot() const = 0;

    virtual const IAccessible* lastFocused() const = 0;

    virtual bool needToVoicePanelInfo() const = 0;
    virtual QString currentPanelAccessibleName() const = 0;

    virtual void setIgnoreQtAccessibilityEvents(bool ignore) = 0;
};
}

#endif // MU_ACCESSIBILITY_IACCESSIBILITYCONTROLLER_H
