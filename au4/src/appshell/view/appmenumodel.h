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
#ifndef AU_APPSHELL_APPMENUMODEL_H
#define AU_APPSHELL_APPMENUMODEL_H

#include <QWindow>

#ifndef MU_QT5_COMPAT
Q_MOC_INCLUDE(< QWindow >)
#endif

#include "uicomponents/view/abstractmenumodel.h"

#include "modularity/ioc.h"
#include "ui/imainwindow.h"
#include "ui/iuiactionsregister.h"
#include "ui/inavigationcontroller.h"
#include "ui/iuiconfiguration.h"
#include "actions/iactionsdispatcher.h"
#include "iappshellconfiguration.h"
#include "internal/iappmenumodelhook.h"
#include "global/iglobalconfiguration.h"

//! TODO AU4
// #include "workspace/iworkspacemanager.h"
// #include "project/irecentfilescontroller.h"
// #include "extensions/iextensionsprovider.h"
// #include "update/iupdateconfiguration.h"

namespace au::appshell {
class AppMenuModel : public mu::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    INJECT(mu::ui::IMainWindow, mainWindow)
    INJECT(mu::ui::IUiActionsRegister, uiActionsRegister)
    INJECT(mu::ui::INavigationController, navigationController)
    INJECT(mu::ui::IUiConfiguration, uiConfiguration)
    INJECT(mu::actions::IActionsDispatcher, actionsDispatcher)
    INJECT(mu::IGlobalConfiguration, globalConfiguration)
    INJECT(IAppShellConfiguration, configuration)
    INJECT(IAppMenuModelHook, appMenuModelHook)

//! TODO AU4
    // INJECT(workspace::IWorkspaceManager, workspacesManager)
    // INJECT(project::IRecentFilesController, recentFilesController)
    // INJECT(extensions::IExtensionsProvider, extensionsProvider)
    // INJECT(update::IUpdateConfiguration, updateConfiguration)

public:
    explicit AppMenuModel(QObject* parent = nullptr);

    Q_INVOKABLE void load() override;
    Q_INVOKABLE bool isGlobalMenuAvailable();

private:
    void setupConnections();

    using mu::uicomponents::AbstractMenuModel::makeMenuItem;
    mu::uicomponents::MenuItem* makeMenuItem(const mu::actions::ActionCode& actionCode, mu::uicomponents::MenuItemRole role);

    mu::uicomponents::MenuItem* makeFileMenu();
    mu::uicomponents::MenuItem* makeEditMenu();
    mu::uicomponents::MenuItem* makeViewMenu();
    mu::uicomponents::MenuItem* makeAddMenu();
    mu::uicomponents::MenuItem* makeFormatMenu();
    mu::uicomponents::MenuItem* makeToolsMenu();
    mu::uicomponents::MenuItem* makePluginsMenu();
    mu::uicomponents::MenuItemList makePluginsMenuSubitems();
    mu::uicomponents::MenuItem* makeHelpMenu();
    mu::uicomponents::MenuItem* makeDiagnosticMenu();

    mu::uicomponents::MenuItemList makeRecentScoresItems();
    mu::uicomponents::MenuItemList appendClearRecentSection(const mu::uicomponents::MenuItemList& recentScores);

    mu::uicomponents::MenuItemList makeNotesItems();
    mu::uicomponents::MenuItemList makeIntervalsItems();
    mu::uicomponents::MenuItemList makeTupletsItems();
    mu::uicomponents::MenuItemList makeMeasuresItems();
    mu::uicomponents::MenuItemList makeFramesItems();
    mu::uicomponents::MenuItemList makeTextItems();
    mu::uicomponents::MenuItemList makeLinesItems();
    mu::uicomponents::MenuItemList makeToolbarsItems();
    mu::uicomponents::MenuItemList makeWorkspacesItems();
    mu::uicomponents::MenuItemList makeShowItems();
    mu::uicomponents::MenuItemList makePluginsItems();
};
}

#endif // AU_APPSHELL_APPMENUMODEL_H
