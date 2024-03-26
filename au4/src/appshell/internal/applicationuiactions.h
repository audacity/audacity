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

//! TODO AU4
// #include "view/preferences/braillepreferencesmodel.h"

#include "view/dockwindow/idockwindowprovider.h"

namespace au::appshell {
class ApplicationUiActions : public mu::ui::IUiActionsModule, public mu::async::Asyncable
{
    INJECT(mu::ui::IMainWindow, mainWindow)
    INJECT(mu::dock::IDockWindowProvider, dockWindowProvider)
    INJECT(IAppShellConfiguration, configuration)
//! TODO AU4
//    INJECT(braille::IBrailleConfiguration, brailleConfiguration)

public:
    ApplicationUiActions(std::shared_ptr<ApplicationActionController> controller);

    void init();

    const mu::ui::UiActionList& actionsList() const override;

    bool actionEnabled(const mu::ui::UiAction& act) const override;
    mu::async::Channel<mu::actions::ActionCodeList> actionEnabledChanged() const override;

    bool actionChecked(const mu::ui::UiAction& act) const override;
    mu::async::Channel<mu::actions::ActionCodeList> actionCheckedChanged() const override;

    static const QMap<mu::actions::ActionCode, DockName>& toggleDockActions();

private:
    void listenOpenedDocksChanged(mu::dock::IDockWindow* window);

    static const mu::ui::UiActionList m_actions;

    std::shared_ptr<ApplicationActionController> m_controller;
    mu::async::Channel<mu::actions::ActionCodeList> m_actionEnabledChanged;
    mu::async::Channel<mu::actions::ActionCodeList> m_actionCheckedChanged;
};
}

#endif // AU_APPSHELL_APPLICATIONUIACTIONS_H
