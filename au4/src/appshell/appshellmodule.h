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

#ifndef AU_APPSHELL_APPSHELLMODULE_H
#define AU_APPSHELL_APPSHELLMODULE_H

#include <memory>

#include <qglobal.h>

#include "modularity/imodulesetup.h"

namespace au::appshell {
class ApplicationActionController;
class ApplicationUiActions;
class AppShellConfiguration;
class SessionsManager;
#ifdef Q_OS_MAC
class MacOSScrollingHook;
 #endif
class AppShellModule : public muse::modularity::IModuleSetup
{
public:
    AppShellModule();

    std::string moduleName() const override;

    void registerExports() override;
    void resolveImports() override;

    void registerResources() override;
    void registerUiTypes() override;

    void onPreInit(const muse::IApplication::RunMode& mode) override;
    void onInit(const muse::IApplication::RunMode& mode) override;
    void onAllInited(const muse::IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    std::shared_ptr<ApplicationActionController> m_applicationActionController;
    std::shared_ptr<ApplicationUiActions> m_applicationUiActions;
    std::shared_ptr<AppShellConfiguration> m_appShellConfiguration;
    std::shared_ptr<SessionsManager> m_sessionsManager;

    #ifdef Q_OS_MAC
    std::shared_ptr<MacOSScrollingHook> m_scrollingHook;
    #endif
};
}

#endif // AU_APPSHELL_APPSHELLMODULE_H
