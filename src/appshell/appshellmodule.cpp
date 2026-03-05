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

static const std::string mname("appshell");

AppShellModule::AppShellModule()
{
}

std::string AppShellModule::moduleName() const
{
    return mname;
}

void AppShellModule::registerExports()
{
    m_appShellConfiguration = std::make_shared<AppShellConfiguration>(muse::modularity::globalCtx());

    globalIoc()->registerExport<IAppShellConfiguration>(mname, m_appShellConfiguration);

#ifdef Q_OS_MAC
    globalIoc()->registerExport<IAppMenuModelHook>(mname, std::make_shared<MacOSAppMenuModelHook>());
#else
    globalIoc()->registerExport<IAppMenuModelHook>(mname, std::make_shared<AppMenuModelHookStub>());
#endif
}

void AppShellModule::resolveImports()
{
    auto ir = globalIoc()->resolve<muse::interactive::IInteractiveUriRegister>(mname);
    if (ir) {
        ir->registerPageUri(muse::Uri("audacity://home"));
        ir->registerPageUri(muse::Uri("audacity://project"));
        ir->registerPageUri(muse::Uri("audacity://publish"));
        ir->registerPageUri(muse::Uri("audacity://devtools"));

        ir->registerQmlUri(muse::Uri("audacity://about/audacity"), "Audacity.AppShell", "AboutDialog");
        ir->registerQmlUri(muse::Uri("audacity://firstLaunchSetup"), "Audacity.AppShell", "FirstLaunchSetupDialog");
        ir->registerQmlUri(muse::Uri("audacity://signin/audiocom"), "Audacity.AppShell", "SigninAudiocomDialog");
        ir->registerQmlUri(muse::Uri("audacity://welcomedialog"), "Audacity.AppShell", "WelcomeDialog");
    }
}

void AppShellModule::onInit(const muse::IApplication::RunMode& mode)
{
    if (mode == muse::IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_appShellConfiguration->init();
}

muse::modularity::IContextSetup* AppShellModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new AppShellContext(ctx);
}

// =====================================================
// AppShellContext
// =====================================================

void AppShellContext::registerExports()
{
    m_applicationActionController = std::make_shared<ApplicationActionController>(iocContext());
    m_applicationUiActions = std::make_shared<ApplicationUiActions>(iocContext(), m_applicationActionController);
    m_sessionsManager = std::make_shared<SessionsManager>(iocContext());

    ioc()->registerExport<IApplicationActionController>(mname, m_applicationActionController);
    ioc()->registerExport<IStartupScenario>(mname, new StartupScenario(iocContext()));
    ioc()->registerExport<ISessionsManager>(mname, m_sessionsManager);
}

void AppShellContext::onPreInit(const muse::IApplication::RunMode& mode)
{
    if (mode == muse::IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_applicationActionController->preInit();
}

void AppShellContext::onInit(const muse::IApplication::RunMode& mode)
{
    if (mode == muse::IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_applicationActionController->init();
    m_applicationUiActions->init();
    m_sessionsManager->init();

    auto ar = ioc()->resolve<muse::ui::IUiActionsRegister>(mname);
    if (ar) {
        ar->reg(m_applicationUiActions);
    }
}

void AppShellContext::onAllInited(const muse::IApplication::RunMode& mode)
{
    if (mode == muse::IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    //! NOTE: process QEvent::FileOpen as early as possible if it was postponed
#ifdef Q_OS_MACOS
    qApp->processEvents();
#endif
}

void AppShellContext::onDeinit()
{
    m_sessionsManager->deinit();
}
