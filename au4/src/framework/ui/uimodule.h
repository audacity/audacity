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

#ifndef MU_UI_UIMODULE_H
#define MU_UI_UIMODULE_H

#include "modularity/imodulesetup.h"
#include <QtGlobal>

namespace mu::ui {
class UiConfiguration;
class UiActionsRegister;
class NavigationController;
class NavigationUiActions;

#ifdef Q_OS_MAC
class MacOSPlatformTheme;
#elif defined(Q_OS_WIN)
class WindowsPlatformTheme;
#elif defined(Q_OS_LINUX)
class LinuxPlatformTheme;
#else
class StubPlatformTheme;
#endif

class UiModule : public modularity::IModuleSetup
{
public:
    std::string moduleName() const override;

    void registerExports() override;
    void resolveImports() override;
    void registerApi() override;
    void registerResources() override;
    void registerUiTypes() override;
    void onPreInit(const IApplication::RunMode& mode) override;
    void onInit(const IApplication::RunMode& mode) override;
    void onAllInited(const IApplication::RunMode& mode) override;
    void onDeinit() override;

private:
    std::shared_ptr<UiConfiguration> m_configuration;
    std::shared_ptr<UiActionsRegister> m_uiactionsRegister;
    std::shared_ptr<NavigationController> m_keyNavigationController;
    std::shared_ptr<NavigationUiActions> m_keyNavigationUiActions;

    #ifdef Q_OS_MAC
    std::shared_ptr<MacOSPlatformTheme> m_platformTheme;
    #elif defined(Q_OS_WIN)
    std::shared_ptr<WindowsPlatformTheme> m_platformTheme;
    #elif defined(Q_OS_LINUX)
    std::shared_ptr<LinuxPlatformTheme> m_platformTheme;
    #else
    std::shared_ptr<StubPlatformTheme> m_platformTheme;
    #endif
};
}

#endif // MU_UI_UIMODULE_H
