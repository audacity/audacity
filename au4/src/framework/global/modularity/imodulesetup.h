/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2024 MuseScore BVBA and others
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

#ifndef MU_MODULARITY_IMODULESETUP_H
#define MU_MODULARITY_IMODULESETUP_H

#include <string>

#include "../iapplication.h"

namespace mu::modularity {
class IModuleSetup
{
public:

    virtual ~IModuleSetup() {}

    virtual std::string moduleName() const = 0;

    virtual void registerExports() {}
    virtual void resolveImports() {}

    virtual void registerResources() {}
    virtual void registerUiTypes() {}
    virtual void registerApi() {}

    virtual void onPreInit(const IApplication::RunMode& mode) { (void)mode; }
    virtual void onInit(const IApplication::RunMode& mode) { (void)mode; }
    virtual void onAllInited(const IApplication::RunMode& mode) { (void)mode; }
    virtual void onDelayedInit() {}
    virtual void onDeinit() {}
    virtual void onDestroy() {}

    virtual void onStartApp() {}
};
}

#endif // MU_MODULARITY_IMODULESETUP_H
