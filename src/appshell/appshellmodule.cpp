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

#include "appshellmodule.h"

#include <QQmlEngine>

#include "framework/global/modularity/ioc.h"

#include "framework/interactive/iinteractiveuriregister.h"
#include "framework/ui/iuiactionsregister.h"

#include "internal/applicationuiactions.h"
#include "internal/applicationactioncontroller.h"
#include "internal/appshellconfiguration.h"
#include "internal/startupscenario.h"
#include "internal/sessionsmanager.h"

#ifdef Q_OS_MAC
#include "internal/platform/macos/macosappmenumodelhook.h"
#else
#include "internal/iappmenumodelhook.h"
#endif

using namespace au::appshell;
using namespace au::appshell;

AppShellModule::AppShellModule()
{
}

std::string AppShellModule::moduleName() const
{
    return "appshell";
}

void AppShellModule::registerExports()
{
    m_applicationActionController = std::make_shared<ApplicationActionController>(iocContext());
    m_applicationUiActions = std::make_shared<ApplicationUiActions>(iocContext(), m_applicationActionController);
    m_appShellConfiguration = std::make_shared<AppShellConfiguration>();
    m_sessionsManager = std::make_shared<SessionsManager>(iocContext());

    ioc()->registerExport<IAppShellConfiguration>(moduleName(), m_appShellConfiguration);
    ioc()->registerExport<IApplicationActionController>(moduleName(), m_applicationActionController);
    ioc()->registerExport<IStartupScenario>(moduleName(), new StartupScenario(iocContext()));
    ioc()->registerExport<ISessionsManager>(moduleName(), m_sessionsManager);

#ifdef Q_OS_MAC
    ioc()->registerExport<IAppMenuModelHook>(moduleName(), std::make_shared<MacOSAppMenuModelHook>());
#else
    ioc()->registerExport<IAppMenuModelHook>(moduleName(), std::make_shared<AppMenuModelHookStub>());
#endif
}

void AppShellModule::resolveImports()
{
    auto ar = ioc()->resolve<muse::ui::IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(m_applicationUiActions);
    }

    auto ir = ioc()->resolve<muse::interactive::IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerPageUri(muse::Uri("audacity://home"));
        ir->registerPageUri(muse::Uri("audacity://project"));
        ir->registerPageUri(muse::Uri("audacity://publish"));
        ir->registerPageUri(muse::Uri("audacity://devtools"));

        ir->registerQmlUri(muse::Uri("audacity://about/audacity"), "Audacity.AppShell", "AboutDialog");
        ir->registerQmlUri(muse::Uri("audacity://firstLaunchSetup"), "Audacity.AppShell", "FirstLaunchSetupDialog");
        ir->registerQmlUri(muse::Uri("audacity://alphaWelcomePopup"), "Audacity.AppShell", "AlphaWelcomePopupDialog");
    }
}

void AppShellModule::onPreInit(const muse::IApplication::RunMode& mode)
{
    if (mode == muse::IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_applicationActionController->preInit();
}

void AppShellModule::onInit(const muse::IApplication::RunMode& mode)
{
    if (mode == muse::IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_appShellConfiguration->init();
    m_applicationActionController->init();
    m_applicationUiActions->init();
    m_sessionsManager->init();
}

void AppShellModule::onAllInited(const muse::IApplication::RunMode& mode)
{
    if (mode == muse::IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    //! NOTE: process QEvent::FileOpen as early as possible if it was postponed
#ifdef Q_OS_MACOS
    qApp->processEvents();
#endif
}

void AppShellModule::onDeinit()
{
    m_sessionsManager->deinit();
}
