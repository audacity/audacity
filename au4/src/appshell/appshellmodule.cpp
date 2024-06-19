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

#include "modularity/ioc.h"

#include "ui/iuiactionsregister.h"
#include "ui/iinteractiveuriregister.h"

#include "internal/applicationuiactions.h"
#include "internal/applicationactioncontroller.h"
#include "internal/appshellconfiguration.h"
#include "internal/startupscenario.h"
#include "internal/sessionsmanager.h"

#include "view/devtools/settingslistmodel.h"
#include "view/mainwindowtitleprovider.h"
#include "view/projectpagemodel.h"
//#include "view/notationstatusbarmodel.h"
#include "view/aboutmodel.h"
#include "view/firstlaunchsetup/firstlaunchsetupmodel.h"
#include "view/firstlaunchsetup/themespagemodel.h"
#include "view/preferences/preferencesmodel.h"
//#include "view/preferences/generalpreferencesmodel.h"
// #include "view/preferences/updatepreferencesmodel.h"
#include "view/preferences/appearancepreferencesmodel.h"
// #include "view/preferences/folderspreferencesmodel.h"
// #include "view/preferences/noteinputpreferencesmodel.h"
// #include "view/preferences/advancedpreferencesmodel.h"
// #include "view/preferences/canvaspreferencesmodel.h"
// #include "view/preferences/saveandpublishpreferencesmodel.h"
// #include "view/preferences/scorepreferencesmodel.h"
// #include "view/preferences/importpreferencesmodel.h"
#include "view/preferences/playbackpreferencesmodel.h"
#include "view/preferences/commonaudioapiconfigurationmodel.h"
// #include "view/preferences/braillepreferencesmodel.h"
#include "view/framelesswindow/framelesswindowmodel.h"
#include "view/publish/publishtoolbarmodel.h"
#include "view/windowdroparea.h"
#include "view/internal/maintoolbarmodel.h"

#ifdef Q_OS_MAC
#include "view/appmenumodel.h"
#include "view/internal/platform/macos/macosappmenumodelhook.h"
#include "view/internal/platform/macos/macosscrollinghook.h"
#else
#include "view/navigableappmenumodel.h"
#endif

using namespace au::appshell;
using namespace au::appshell;
using namespace mu;
using namespace muse;
using namespace muse::modularity;
using namespace muse::ui;

static void appshell_init_qrc()
{
    Q_INIT_RESOURCE(appshell);
}

AppShellModule::AppShellModule()
{
}

std::string AppShellModule::moduleName() const
{
    return "appshell";
}

void AppShellModule::registerExports()
{
    m_applicationActionController = std::make_shared<ApplicationActionController>();
    m_applicationUiActions = std::make_shared<ApplicationUiActions>(m_applicationActionController);
    m_appShellConfiguration = std::make_shared<AppShellConfiguration>();
    m_sessionsManager = std::make_shared<SessionsManager>();

    #ifdef Q_OS_MAC
    m_scrollingHook = std::make_shared<MacOSScrollingHook>();
    #endif

    ioc()->registerExport<IAppShellConfiguration>(moduleName(), m_appShellConfiguration);
    ioc()->registerExport<IApplicationActionController>(moduleName(), m_applicationActionController);
    ioc()->registerExport<IStartupScenario>(moduleName(), new StartupScenario());
    ioc()->registerExport<ISessionsManager>(moduleName(), m_sessionsManager);

#ifdef Q_OS_MAC
    ioc()->registerExport<IAppMenuModelHook>(moduleName(), std::make_shared<MacOSAppMenuModelHook>());
#else
    ioc()->registerExport<IAppMenuModelHook>(moduleName(), std::make_shared<AppMenuModelHookStub>());
#endif
}

void AppShellModule::resolveImports()
{
    auto ar = ioc()->resolve<ui::IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(m_applicationUiActions);
    }

    auto ir = ioc()->resolve<IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerUri(Uri("musescore://home"), ContainerMeta(ContainerType::PrimaryPage));
        ir->registerUri(Uri("audacity://project"), ContainerMeta(ContainerType::PrimaryPage));
        ir->registerUri(Uri("musescore://sequencer"), ContainerMeta(ContainerType::PrimaryPage));
        ir->registerUri(Uri("musescore://publish"), ContainerMeta(ContainerType::PrimaryPage));
        ir->registerUri(Uri("musescore://devtools"), ContainerMeta(ContainerType::PrimaryPage));
        ir->registerUri(Uri("musescore://about/musescore"), ContainerMeta(ContainerType::QmlDialog, "AboutDialog.qml"));
        ir->registerUri(Uri("musescore://about/musicxml"), ContainerMeta(ContainerType::QmlDialog, "AboutMusicXMLDialog.qml"));
        ir->registerUri(Uri("musescore://firstLaunchSetup"),
                        ContainerMeta(ContainerType::QmlDialog, "FirstLaunchSetup/FirstLaunchSetupDialog.qml"));
        ir->registerUri(Uri("audacity://preferences"), ContainerMeta(ContainerType::QmlDialog, "Preferences/PreferencesDialog.qml"));
    }
}

void AppShellModule::registerResources()
{
    appshell_init_qrc();
}

void AppShellModule::registerUiTypes()
{
    qmlRegisterType<SettingListModel>("Audacity.Preferences", 1, 0, "SettingListModel");
    qmlRegisterType<PreferencesModel>("Audacity.Preferences", 1, 0, "PreferencesModel");
    // qmlRegisterType<GeneralPreferencesModel>("MuseScore.Preferences", 1, 0, "GeneralPreferencesModel");
    // qmlRegisterType<UpdatePreferencesModel>("MuseScore.Preferences", 1, 0, "UpdatePreferencesModel");
    qmlRegisterType<AppearancePreferencesModel>("Audacity.Preferences", 1, 0, "AppearancePreferencesModel");
    // qmlRegisterType<FoldersPreferencesModel>("MuseScore.Preferences", 1, 0, "FoldersPreferencesModel");
    // qmlRegisterType<NoteInputPreferencesModel>("MuseScore.Preferences", 1, 0, "NoteInputPreferencesModel");
    // qmlRegisterType<AdvancedPreferencesModel>("MuseScore.Preferences", 1, 0, "AdvancedPreferencesModel");
    // qmlRegisterType<CanvasPreferencesModel>("MuseScore.Preferences", 1, 0, "CanvasPreferencesModel");
    // qmlRegisterType<SaveAndPublishPreferencesModel>("MuseScore.Preferences", 1, 0, "SaveAndPublishPreferencesModel");
    // qmlRegisterType<ScorePreferencesModel>("MuseScore.Preferences", 1, 0, "ScorePreferencesModel");
    // qmlRegisterType<ImportPreferencesModel>("MuseScore.Preferences", 1, 0, "ImportPreferencesModel");
    qmlRegisterType<PlaybackPreferencesModel>("Audacity.Preferences", 1, 0, "PlaybackPreferencesModel");
    qmlRegisterType<CommonAudioApiConfigurationModel>("Audacity.Preferences", 1, 0, "CommonAudioApiConfigurationModel");
    // qmlRegisterType<BraillePreferencesModel>("MuseScore.Preferences", 1, 0, "BraillePreferencesModel");

#if defined(Q_OS_MACOS)
    qmlRegisterType<AppMenuModel>("Audacity.AppShell", 1, 0, "PlatformAppMenuModel");
#elif defined(Q_OS_LINUX) || defined(Q_OS_FREEBSD)
    qmlRegisterType<AppMenuModel>("Audacity.AppShell", 1, 0, "PlatformAppMenuModel");
    qmlRegisterType<NavigableAppMenuModel>("Audacity.AppShell", 1, 0, "AppMenuModel");
#else
    qmlRegisterType<NavigableAppMenuModel>("Audacity.AppShell", 1, 0, "AppMenuModel");
#endif

    qmlRegisterType<MainWindowTitleProvider>("Audacity.AppShell", 1, 0, "MainWindowTitleProvider");
    qmlRegisterType<ProjectPageModel>("Audacity.AppShell", 1, 0, "ProjectPageModel");
//    qmlRegisterType<NotationStatusBarModel>("Audacity.AppShell", 1, 0, "NotationStatusBarModel");
    qmlRegisterType<AboutModel>("Audacity.AppShell", 1, 0, "AboutModel");
    qmlRegisterType<FirstLaunchSetupModel>("Audacity.AppShell", 1, 0, "FirstLaunchSetupModel");
    qmlRegisterType<ThemesPageModel>("Audacity.AppShell", 1, 0, "ThemesPageModel");
    qmlRegisterType<FramelessWindowModel>("Audacity.AppShell", 1, 0, "FramelessWindowModel");
    qmlRegisterType<PublishToolBarModel>("Audacity.AppShell", 1, 0, "PublishToolBarModel");
    qmlRegisterType<MainToolBarModel>("Audacity.AppShell", 1, 0, "MainToolBarModel");

    qmlRegisterType<WindowDropArea>("Muse.Ui", 1, 0, "WindowDropArea");
}

void AppShellModule::onPreInit(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_applicationActionController->preInit();
}

void AppShellModule::onInit(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_appShellConfiguration->init();
    m_applicationActionController->init();
    m_applicationUiActions->init();
    m_sessionsManager->init();

#ifdef Q_OS_MAC
    m_scrollingHook->init();
#endif
}

void AppShellModule::onAllInited(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
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
