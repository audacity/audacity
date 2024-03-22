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
#ifndef MU_APPSHELL_APPMENUMODEL_H
#define MU_APPSHELL_APPMENUMODEL_H

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
#include "workspace/iworkspacemanager.h"
#include "iappshellconfiguration.h"
#include "project/irecentfilescontroller.h"
#include "internal/iappmenumodelhook.h"
#include "extensions/iextensionsprovider.h"
#include "update/iupdateconfiguration.h"
#include "global/iglobalconfiguration.h"

namespace mu::appshell {
class AppMenuModel : public uicomponents::AbstractMenuModel
{
    Q_OBJECT

    INJECT(ui::IMainWindow, mainWindow)
    INJECT(ui::IUiActionsRegister, uiActionsRegister)
    INJECT(ui::INavigationController, navigationController)
    INJECT(ui::IUiConfiguration, uiConfiguration)
    INJECT(actions::IActionsDispatcher, actionsDispatcher)
    INJECT(workspace::IWorkspaceManager, workspacesManager)
    INJECT(IAppShellConfiguration, configuration)
    INJECT(project::IRecentFilesController, recentFilesController)
    INJECT(IAppMenuModelHook, appMenuModelHook)
    INJECT(extensions::IExtensionsProvider, extensionsProvider)
    INJECT(update::IUpdateConfiguration, updateConfiguration)
    INJECT(IGlobalConfiguration, globalConfiguration)

public:
    explicit AppMenuModel(QObject* parent = nullptr);

    Q_INVOKABLE void load() override;
    Q_INVOKABLE bool isGlobalMenuAvailable();

private:
    void setupConnections();

    using uicomponents::AbstractMenuModel::makeMenuItem;
    uicomponents::MenuItem* makeMenuItem(const actions::ActionCode& actionCode, uicomponents::MenuItemRole role);

    uicomponents::MenuItem* makeFileMenu();
    uicomponents::MenuItem* makeEditMenu();
    uicomponents::MenuItem* makeViewMenu();
    uicomponents::MenuItem* makeAddMenu();
    uicomponents::MenuItem* makeFormatMenu();
    uicomponents::MenuItem* makeToolsMenu();
    uicomponents::MenuItem* makePluginsMenu();
    uicomponents::MenuItemList makePluginsMenuSubitems();
    uicomponents::MenuItem* makeHelpMenu();
    uicomponents::MenuItem* makeDiagnosticMenu();

    uicomponents::MenuItemList makeRecentScoresItems();
    uicomponents::MenuItemList appendClearRecentSection(const uicomponents::MenuItemList& recentScores);

    uicomponents::MenuItemList makeNotesItems();
    uicomponents::MenuItemList makeIntervalsItems();
    uicomponents::MenuItemList makeTupletsItems();
    uicomponents::MenuItemList makeMeasuresItems();
    uicomponents::MenuItemList makeFramesItems();
    uicomponents::MenuItemList makeTextItems();
    uicomponents::MenuItemList makeLinesItems();
    uicomponents::MenuItemList makeToolbarsItems();
    uicomponents::MenuItemList makeWorkspacesItems();
    uicomponents::MenuItemList makeShowItems();
    uicomponents::MenuItemList makePluginsItems();
};
}

#endif // MU_APPSHELL_APPMENUMODEL_H
