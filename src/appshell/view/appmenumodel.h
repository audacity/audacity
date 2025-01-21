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
#include "effects/effects_base/ieffectsprovider.h"

//! TODO AU4
// #include "workspace/iworkspacemanager.h"
#include "project/irecentfilescontroller.h"
// #include "extensions/iextensionsprovider.h"
// #include "update/iupdateconfiguration.h"

namespace au::appshell {
class AppMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

public:
    INJECT(muse::ui::IMainWindow, mainWindow)
    INJECT(muse::ui::IUiActionsRegister, uiActionsRegister)
    INJECT(muse::ui::INavigationController, navigationController)
    INJECT(muse::ui::IUiConfiguration, uiConfiguration)
    INJECT(muse::actions::IActionsDispatcher, actionsDispatcher)
    INJECT(muse::IGlobalConfiguration, globalConfiguration)
    INJECT(IAppShellConfiguration, configuration)
    INJECT(IAppMenuModelHook, appMenuModelHook)
    INJECT(effects::IEffectsProvider, effectsProvider);

//! TODO AU4
    // INJECT(workspace::IWorkspaceManager, workspacesManager)
    INJECT(au::project::IRecentFilesController, recentFilesController)
    // INJECT(extensions::IExtensionsProvider, extensionsProvider)
    // INJECT(update::IUpdateConfiguration, updateConfiguration)

public:
    explicit AppMenuModel(QObject* parent = nullptr);

    Q_INVOKABLE void load() override;
    Q_INVOKABLE bool isGlobalMenuAvailable();

private:
    void setupConnections();

    using muse::uicomponents::AbstractMenuModel::makeMenuItem;
    muse::uicomponents::MenuItem* makeMenuItem(const muse::actions::ActionCode& actionCode, muse::uicomponents::MenuItemRole role);

    muse::uicomponents::MenuItem* makeFileMenu();
    muse::uicomponents::MenuItem* makeEditMenu();
    muse::uicomponents::MenuItem* makeSelectMenu();
    muse::uicomponents::MenuItem* makeViewMenu();
    muse::uicomponents::MenuItem* makeRecordMenu();
    muse::uicomponents::MenuItem* makeTracksMenu();
    muse::uicomponents::MenuItem* makeGenerateMenu();
    muse::uicomponents::MenuItem* makeEffectMenu();
    muse::uicomponents::MenuItem* makeAnalyzeMenu();
    muse::uicomponents::MenuItem* makeToolsMenu();
    muse::uicomponents::MenuItem* makeExtraMenu();
    muse::uicomponents::MenuItem* makeHelpMenu();
    muse::uicomponents::MenuItem* makeDiagnosticMenu();

    muse::uicomponents::MenuItemList makeRecentProjectsItems();
    muse::uicomponents::MenuItemList appendClearRecentSection(const muse::uicomponents::MenuItemList& recentScores);

    muse::uicomponents::MenuItemList makeExportItems();
    muse::uicomponents::MenuItemList makeClipItems();
    muse::uicomponents::MenuItemList makeAudioActionsItems();
    muse::uicomponents::MenuItemList makeLoopingItems();
    muse::uicomponents::MenuItemList makeZoomItems();
    muse::uicomponents::MenuItemList makeSkipToItems();
    muse::uicomponents::MenuItemList makeAlignItems();
    muse::uicomponents::MenuItemList makeSortItems();
    muse::uicomponents::MenuItemList makeVolumeAndCompressionItems();
    muse::uicomponents::MenuItemList makeMacrosItems();
    muse::uicomponents::MenuItemList makeDiagnosticsItems();
    muse::uicomponents::MenuItemList makeFramesItems();
    muse::uicomponents::MenuItemList makeWorkspacesItems();
    muse::uicomponents::MenuItemList makeShowItems();
    muse::uicomponents::MenuItemList makeEffectsItems();

    void setItemIsChecked(const QString& itemId, bool checked);
};
}

#endif // AU_APPSHELL_APPMENUMODEL_H
