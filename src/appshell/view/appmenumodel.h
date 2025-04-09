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
#include "effects/effects_base/ieffectsconfiguration.h"
#include "effects/effects_base/effectstypes.h"
#include "trackedit/iprojecthistory.h"

//! TODO AU4
// #include "workspace/iworkspacemanager.h"
#include "project/irecentfilescontroller.h"
// #include "extensions/iextensionsprovider.h"
// #include "update/iupdateconfiguration.h"

namespace au::appshell {
class AppMenuModel : public muse::uicomponents::AbstractMenuModel, public effects::IEffectMenuItemFactory
{
    Q_OBJECT

public:
    muse::Inject<muse::ui::IMainWindow> mainWindow = { this };
    muse::Inject<muse::ui::IUiActionsRegister> uiActionsRegister = { this };
    muse::Inject<muse::ui::INavigationController> navigationController = { this };
    muse::Inject<muse::ui::IUiConfiguration> uiConfiguration = { this };
    muse::Inject<muse::actions::IActionsDispatcher> actionsDispatcher = { this };
    muse::Inject<muse::IGlobalConfiguration> globalConfiguration = { this };
    muse::Inject<IAppShellConfiguration> configuration = { this };
    muse::Inject<IAppMenuModelHook> appMenuModelHook = { this };
    muse::Inject<effects::IEffectsProvider> effectsProvider = { this };
    muse::Inject<effects::IEffectsConfiguration> effectsConfiguration = { this };
    muse::Inject<trackedit::IProjectHistory> projectHistory = { this };

    //! TODO AU4
    // muse::Inject<workspace::IWorkspaceManager> workspacesManager = { this };
    muse::Inject<au::project::IRecentFilesController> recentFilesController = { this };
    // muse::Inject<extensions::IExtensionsProvider> extensionsProvider = { this };
    // muse::Inject<update::IUpdateConfiguration> updateConfiguration = { this };

public:
    explicit AppMenuModel(QObject* parent = nullptr);

    Q_INVOKABLE void load() override;
    Q_INVOKABLE bool isGlobalMenuAvailable();

private:
    void setupConnections();
    void onEffectsChanged();

    // effects::IEffectMenuItemFactory
    muse::uicomponents::MenuItem* makeMenuSeparator() override { return makeSeparator(); }
    muse::uicomponents::MenuItem* makeMenuEffectItem(const effects::EffectId& effectId) override;
    muse::uicomponents::MenuItem* makeMenuEffect(const muse::String& title, const muse::uicomponents::MenuItemList& items) override;

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
    muse::uicomponents::MenuItemList makeGeneratorItems();

    void setItemIsChecked(const QString& itemId, bool checked);

    void updateUndoRedoItems();

    std::shared_ptr<muse::uicomponents::AbstractMenuModel> m_workspacesMenuModel;
};
}

#endif // AU_APPSHELL_APPMENUMODEL_H
