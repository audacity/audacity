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
#ifndef AU_APPSHELL_IAPPMENUMODELHOOK_H
#define AU_APPSHELL_IAPPMENUMODELHOOK_H

#include "modularity/imoduleinterface.h"

namespace au::appshell {
class IAppMenuModelHook : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAppMenuModelHook)

public:
    virtual ~IAppMenuModelHook() = default;

    virtual void onAppMenuInited() = 0;
};

class AppMenuModelHookStub : public IAppMenuModelHook
{
public:
    void onAppMenuInited() override {}
};
}

#endif // AU_APPSHELL_IAPPMENUMODELHOOK_H
