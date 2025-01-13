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
#ifndef AU_APPSHELL_APPLICATIONUIACTIONS_H
#define AU_APPSHELL_APPLICATIONUIACTIONS_H

#include "ui/iuiactionsmodule.h"
#include "applicationactioncontroller.h"
#include "modularity/ioc.h"
#include "context/iuicontextresolver.h"
#include "async/asyncable.h"
#include "ui/imainwindow.h"
#include "record/irecordcontroller.h"

//! TODO AU4
// #include "view/preferences/braillepreferencesmodel.h"

#include "dockwindow/idockwindowprovider.h"

namespace au::appshell {
class ApplicationUiActions : public muse::ui::IUiActionsModule, public muse::async::Asyncable
{
    INJECT(muse::ui::IMainWindow, mainWindow)
    INJECT(muse::dock::IDockWindowProvider, dockWindowProvider)
    INJECT(IAppShellConfiguration, configuration)
    INJECT(record::IRecordController, recordController)
//! TODO AU4
//    INJECT(braille::IBrailleConfiguration, brailleConfiguration)

public:
    ApplicationUiActions(std::shared_ptr<ApplicationActionController> controller);

    void init();

    const muse::ui::UiActionList& actionsList() const override;

    bool actionEnabled(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionEnabledChanged() const override;

    bool actionChecked(const muse::ui::UiAction& act) const override;
    muse::async::Channel<muse::actions::ActionCodeList> actionCheckedChanged() const override;

    static const QMap<muse::actions::ActionCode, DockName>& toggleDockActions();

private:
    void listenOpenedDocksChanged(muse::dock::IDockWindow* window);

    static const muse::ui::UiActionList m_actions;

    std::shared_ptr<ApplicationActionController> m_controller;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionEnabledChanged;
    muse::async::Channel<muse::actions::ActionCodeList> m_actionCheckedChanged;
};
}

#endif // AU_APPSHELL_APPLICATIONUIACTIONS_H
