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

#include "modularity/ioc.h"
#include "actions/actionable.h"
#include "actions/iactionsdispatcher.h"
#include "ui/iuiactionsregister.h"
#include "async/asyncable.h"
#include "ui/imainwindow.h"
#include "iinteractive.h"
#include "iappshellconfiguration.h"
#include "iapplication.h"
#include "project/iprojectfilescontroller.h"
#include "record/irecordcontroller.h"

//! TODO AU4
// #include "languages/ilanguagesservice.h"
// #include "multiinstances/imultiinstancesprovider.h"
// #include "audio/isoundfontrepository.h"
// #include "istartupscenario.h"

namespace au::appshell {
class ApplicationActionController : public QObject, public IApplicationActionController, public muse::actions::Actionable,
    public muse::async::Asyncable
{
    INJECT(muse::actions::IActionsDispatcher, dispatcher)
    INJECT(muse::ui::IUiActionsRegister, actionsRegister)
    INJECT(muse::ui::IMainWindow, mainWindow)

    INJECT(muse::IInteractive, interactive)
    INJECT(muse::IApplication, application)
    INJECT(IAppShellConfiguration, configuration)
    INJECT(project::IProjectFilesController, projectFilesController)
    INJECT(record::IRecordController, recordController)
//! TODO AU4
    // INJECT(languages::ILanguagesService, languagesService)
    // INJECT(mi::IMultiInstancesProvider, multiInstancesProvider)
    // INJECT(audio::ISoundFontRepository, soundFontRepository)
    // INJECT(IStartupScenario, startupScenario)
public:
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

    bool quit();
    void restart();

    void toggleFullScreen();
    void openAboutDialog();
    void openAboutQtDialog();

    void openOnlineHandbookPage();
    void openAskForHelpPage();
    void openPreferencesDialog();

    void revertToFactorySettings();

    bool m_quiting = false;

    muse::async::Channel<muse::actions::ActionCodeList> m_actionsReceiveAvailableChanged;
};
}

#endif // AU_APPSHELL_APPLICATIONCONTROLLER_H
