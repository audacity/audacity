/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
 *
 * Copyright (C) 2024 Audacity BVBA and others
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
#ifndef AU_APPSHELL_APPLICATIONCONTROLLER_H
#define AU_APPSHELL_APPLICATIONCONTROLLER_H

#include <QObject>

#include "../iapplicationactioncontroller.h"

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/actions/actionable.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/interactive/iplatforminteractive.h"
#include "framework/ui/iuiactionsregister.h"
#include "framework/ui/imainwindow.h"

#include "iappshellconfiguration.h"
#include "iapplication.h"
#include "startupscenario.h"
#include "project/iprojectfilescontroller.h"
#include "record/irecordcontroller.h"
#include "context/iuicontextresolver.h"
#include "context/iglobalcontext.h"
//! TODO AU4
// #include "languages/ilanguagesservice.h"
#include "multiwindows/imultiwindowsprovider.h"
// #include "audio/isoundfontrepository.h"
// #include "istartupscenario.h"

namespace au::appshell {
class ApplicationActionController : public QObject, public IApplicationActionController, public muse::actions::Actionable,
    public muse::async::Asyncable, public muse::Contextable
{
    muse::GlobalInject<muse::IApplication> application;
    muse::GlobalInject<IAppShellConfiguration> configuration;
    muse::GlobalInject<muse::IPlatformInteractive> platformInteractive;
    muse::GlobalInject<muse::mi::IMultiWindowsProvider> multiwindowsProvider;

    muse::ContextInject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::ContextInject<muse::ui::IUiActionsRegister> actionsRegister { this };
    muse::ContextInject<muse::ui::IMainWindow> mainWindow { this };
    muse::ContextInject<muse::IInteractive> interactive { this };
    muse::ContextInject<appshell::IStartupScenario> startupScenario { this };
    muse::ContextInject<project::IProjectFilesController> projectFilesController { this };
    muse::ContextInject<record::IRecordController> recordController { this };
    muse::ContextInject<context::IUiContextResolver> uiContextResolver { this };
    muse::ContextInject<context::IGlobalContext> globalContext { this };

public:

    friend class FactoryResetActionTests;

    ApplicationActionController(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void preInit();
    void init();
    const std::vector<muse::actions::ActionCode>& prohibitedActionsWhileRecording() const;

    muse::ValCh<bool> isFullScreen() const;

    void onDragEnterEvent(QDragEnterEvent* event) override;
    void onDragMoveEvent(QDragMoveEvent* event) override;
    void onDropEvent(QDropEvent* event) override;

    bool canReceiveAction(const muse::actions::ActionCode& code) const override;

private:
    bool eventFilter(QObject* watched, QEvent* event) override;

    void setupConnections();

    bool quit(const muse::io::path_t& installerPath = "");
    void restart();

    void toggleFullScreen();
    void openAboutDialog();
    void openAboutQtDialog();

    void openOnlineHandbookPage();
    void openAskForHelpPage();
    void openPreferencesDialog();
    void openAudioSettingsDialog();
    void openShortcutsPreferencesDialog();
    void openEditingPreferencesDialog();
    void openSpectrogramPreferencesDialog();

    void revertToFactorySettings();

    bool isProjectOpened() const;
    bool isProjectOpenedAndFocused() const;

    void doGlobalCopy();
    void doGlobalCut();
    void doGlobalPaste();
    void doGlobalUndo();
    void doGlobalRedo();
    void doGlobalDelete();
    void doGlobalCancel();
    void doGlobalTrigger();

    bool m_quiting = false;

    muse::async::Channel<muse::actions::ActionCodeList> m_actionsReceiveAvailableChanged;
};
}

#endif // AU_APPSHELL_APPLICATIONCONTROLLER_H
