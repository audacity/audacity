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
#include "uimodule.h"

#include <QtQml>
#include <QFontDatabase>

#include "modularity/ioc.h"

#include "internal/uiengine.h"
#include "internal/mainwindow.h"
#include "internal/uiconfiguration.h"
#include "internal/interactiveuriregister.h"
#include "internal/uiactionsregister.h"
#include "internal/navigationcontroller.h"
#include "internal/navigationuiactions.h"

#ifdef Q_OS_MAC
#include "internal/platform/macos/macosplatformtheme.h"
#include "view/platform/macos/macosmainwindowbridge.h"
#elif defined(Q_OS_WIN)
#include "internal/platform/windows/windowsplatformtheme.h"
#include "view/mainwindowbridge.h"
#elif defined(Q_OS_LINUX)
#include "internal/platform/linux/linuxplatformtheme.h"
#include "view/mainwindowbridge.h"
#else
#include "internal/platform/stub/stubplatformtheme.h"
#include "view/mainwindowbridge.h"
#endif

#include "internal/uiengine.h"

#include "view/qmltooltip.h"
#include "view/iconcodes.h"
#include "view/musicalsymbolcodes.h"
#include "view/navigationsection.h"
#include "view/navigationpanel.h"
#include "view/navigationpopuppanel.h"
#include "view/navigationcontrol.h"
#include "view/navigationevent.h"
#include "view/qmlaccessible.h"
#include "view/focuslistener.h"

#include "view/internal/errordetailsmodel.h"
#include "view/internal/progressdialogmodel.h"

#include "global/api/iapiregister.h"
#include "api/navigationapi.h"
#include "api/keyboardapi.h"

#include "dev/interactivetestsmodel.h"
#include "dev/testdialog.h"

using namespace mu::ui;
using namespace mu::modularity;

static void ui_init_qrc()
{
    Q_INIT_RESOURCE(ui);
}

std::string UiModule::moduleName() const
{
    return "ui";
}

void UiModule::registerExports()
{
    m_configuration = std::make_shared<UiConfiguration>();
    m_uiactionsRegister = std::make_shared<UiActionsRegister>();
    m_keyNavigationController = std::make_shared<NavigationController>();
    m_keyNavigationUiActions = std::make_shared<NavigationUiActions>();

    #ifdef Q_OS_MAC
    m_platformTheme = std::make_shared<MacOSPlatformTheme>();
    #elif defined(Q_OS_WIN)
    m_platformTheme = std::make_shared<WindowsPlatformTheme>();
    #elif defined(Q_OS_LINUX)
    m_platformTheme = std::make_shared<LinuxPlatformTheme>();
    #else
    m_platformTheme = std::make_shared<StubPlatformTheme>();
    #endif

    ioc()->registerExport<IUiConfiguration>(moduleName(), m_configuration);
    ioc()->registerExportNoDelete<IUiEngine>(moduleName(), UiEngine::instance());
    ioc()->registerExport<IMainWindow>(moduleName(), new MainWindow());
    ioc()->registerExport<IInteractiveProvider>(moduleName(), UiEngine::instance()->interactiveProvider());
    ioc()->registerExport<IInteractiveUriRegister>(moduleName(), new InteractiveUriRegister());
    ioc()->registerExport<IPlatformTheme>(moduleName(), m_platformTheme);
    ioc()->registerExport<IUiActionsRegister>(moduleName(), m_uiactionsRegister);
    ioc()->registerExport<INavigationController>(moduleName(), m_keyNavigationController);
}

void UiModule::resolveImports()
{
    auto ar = ioc()->resolve<IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(m_keyNavigationUiActions);
    }

    auto ir = modularity::ioc()->resolve<IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerWidgetUri<TestDialog>(Uri("musescore://devtools/interactive/testdialog"));
        ir->registerQmlUri(Uri("musescore://devtools/interactive/sample"), "DevTools/Interactive/SampleDialog.qml");
    }
}

void UiModule::registerApi()
{
    using namespace mu::api;

    auto api = ioc()->resolve<IApiRegister>(moduleName());
    if (api) {
        api->regApiCreator(moduleName(), "api.navigation", new ApiCreator<NavigationApi>());
        api->regApiCreator(moduleName(), "api.keyboard", new ApiCreator<KeyboardApi>());
        api->regApiSingltone(moduleName(), "api.theme", UiEngine::instance()->theme());
    }
}

void UiModule::registerResources()
{
    ui_init_qrc();
}

void UiModule::registerUiTypes()
{
#ifdef MU_QT5_COMPAT
    qRegisterMetaType<api::ThemeApi*>("api::ThemeApi*");
#endif
    qmlRegisterUncreatableType<UiEngine>("MuseScore.Ui", 1, 0, "UiEngine", "Cannot create an UiEngine");
    qmlRegisterUncreatableType<api::ThemeApi>("MuseScore.Ui", 1, 0, "QmlTheme", "Cannot create a QmlTheme");
    qmlRegisterUncreatableType<QmlToolTip>("MuseScore.Ui", 1, 0, "QmlToolTip", "Cannot create a QmlToolTip");
    qmlRegisterUncreatableType<IconCode>("MuseScore.Ui", 1, 0, "IconCode", "Cannot create an IconCode");
    qmlRegisterUncreatableType<MusicalSymbolCodes>("MuseScore.Ui", 1, 0, "MusicalSymbolCodes",
                                                   "Cannot create an MusicalSymbolCodes");
    qmlRegisterUncreatableType<InteractiveProvider>("MuseScore.Ui", 1, 0, "QmlInteractiveProvider", "Cannot create");
    qmlRegisterUncreatableType<ContainerType>("MuseScore.Ui", 1, 0, "ContainerType", "Cannot create a ContainerType");

    qmlRegisterUncreatableType<NavigationEvent>("MuseScore.Ui", 1, 0, "NavigationEvent", "Cannot create a KeyNavigationEvent");
    qmlRegisterType<NavigationSection>("MuseScore.Ui", 1, 0, "NavigationSection");
    qmlRegisterType<NavigationPanel>("MuseScore.Ui", 1, 0, "NavigationPanel");
    qmlRegisterType<NavigationPopupPanel>("MuseScore.Ui", 1, 0, "NavigationPopupPanel");
    qmlRegisterType<NavigationControl>("MuseScore.Ui", 1, 0, "NavigationControl");
    qmlRegisterType<AccessibleItem>("MuseScore.Ui", 1, 0, "AccessibleItem");
    qmlRegisterUncreatableType<MUAccessible>("MuseScore.Ui", 1, 0, "MUAccessible", "Cannot create a enum type");

    qmlRegisterType<FocusListener>("MuseScore.Ui", 1, 0, "FocusListener");

#ifdef Q_OS_MAC
    qmlRegisterType<MacOSMainWindowBridge>("MuseScore.Ui", 1, 0, "MainWindowBridge");
#else
    qmlRegisterType<MainWindowBridge>("MuseScore.Ui", 1, 0, "MainWindowBridge");
#endif

    qmlRegisterType<ErrorDetailsModel>("MuseScore.Ui", 1, 0, "ErrorDetailsModel");
    qmlRegisterType<ProgressDialogModel>("MuseScore.Ui", 1, 0, "ProgressDialogModel");

    qmlRegisterType<InteractiveTestsModel>("MuseScore.Ui", 1, 0, "InteractiveTestsModel");
    qRegisterMetaType<TestDialog>("TestDialog");

    modularity::ioc()->resolve<ui::IUiEngine>(moduleName())->addSourceImportPath(ui_QML_IMPORT);
}

void UiModule::onPreInit(const IApplication::RunMode& mode)
{
    if (mode == IApplication::RunMode::AudioPluginRegistration) {
        return;
    }

    m_configuration->init();
}

void UiModule::onInit(const IApplication::RunMode& mode)
{
    if (mode != IApplication::RunMode::GuiApp) {
        return;
    }

    QFontDatabase::addApplicationFont(":/fonts/mscore/MusescoreIcon.ttf"); // icons

    m_keyNavigationController->init();
}

void UiModule::onAllInited(const IApplication::RunMode& mode)
{
    if (IApplication::RunMode::GuiApp != mode) {
        return;
    }

    //! NOTE Some of the settings are taken from the workspace,
    //! we need to be sure that the workspaces are initialized.
    //! So, we loads these settings on onStartApp
    m_configuration->load();

    //! NOTE UIActions are collected from many modules, and these modules determine the state of their UIActions.
    //! All modules need to be initialized in order to get the correct state of UIActions.
    //! So, we do init on onStartApp
    m_uiactionsRegister->init();
}

void UiModule::onDeinit()
{
    m_configuration->deinit();
}
