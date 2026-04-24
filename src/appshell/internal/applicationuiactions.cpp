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
#include "applicationuiactions.h"

#include "ui/view/iconcodes.h"
#include "context/uicontext.h"
#include "context/shortcutcontext.h"

#include "dockwindow/idockwindow.h"
#include "async/notification.h"

#include "log.h"

using namespace au::appshell;

using namespace muse;
using namespace muse::ui;
using namespace muse::actions;
using namespace muse::dock;

static const ActionCode FULL_SCREEN_CODE("fullscreen");

const UiActionList ApplicationUiActions::m_actions = {
    UiAction("quit",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Exit"),
             TranslatableString("action", "Exit")
             ),
    UiAction("restart",
             au::context::UiCtxAny,
             au::context::CTX_DISABLED,
             TranslatableString("action", "Restart")
             ),
    UiAction(FULL_SCREEN_CODE,
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Full screen"),
             TranslatableString("action", "Full screen"),
             Checkable::Yes
             ),
    UiAction("about-audacity",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&About Audacity…")
             ),
    UiAction("about-qt",
             au::context::UiCtxAny,
             au::context::CTX_DISABLED,
             TranslatableString("action", "About &Qt…")
             ),
    UiAction("online-handbook",
             au::context::UiCtxUnknown,
             au::context::CTX_DISABLED,
             TranslatableString("action", "Online &handbook"),
             TranslatableString("action", "Open online handbook")
             ),
    UiAction("ask-help",
             au::context::UiCtxAny,
             au::context::CTX_DISABLED,
             TranslatableString("action", "As&k for help")
             ),
    UiAction("revert-factory",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Revert to &factory settings"),
             TranslatableString("action", "Revert to factory settings")
             ),

    // Docking
    UiAction("dock-restore-default-layout",
             au::context::UiCtxAny,
             au::context::CTX_DISABLED,
             TranslatableString("action", "Restore the &default layout"),
             TranslatableString("action", "Restore the default layout")
             ),

    // Toolbars
    UiAction("toggle-transport",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             TranslatableString("action", "&Playback controls"),
             TranslatableString("action", "Show/hide playback controls"),
             Checkable::Yes
             ),
    // Vertical panels
    UiAction("toggle-tracks",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "&Tracks"),
             TranslatableString("action", "Show/hide tracks"),
             Checkable::Yes
             ),
    // Status bar
    UiAction("toggle-statusbar",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             TranslatableString("action", "&Status bar"),
             TranslatableString("action", "Show/hide status bar"),
             Checkable::Yes
             ),

    UiAction("preference-dialog",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Preferences"),
             TranslatableString("action", "Preferences…")
             ),
    UiAction("action://copy",
             { "action://trackedit/copy" },
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Copy"),
             TranslatableString("action", "Copy"),
             IconCode::Code::COPY
             ),
    UiAction("action://cut",
             { "action://trackedit/cut" },
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Cut"),
             TranslatableString("action", "Cut"),
             IconCode::Code::CUT
             ),
    UiAction("action://paste",
             { "action://trackedit/paste-default" },
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Paste"),
             TranslatableString("action", "Paste"),
             IconCode::Code::PASTE
             ),
    UiAction("action://undo",
             { "action://trackedit/undo" },
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Undo"),
             TranslatableString("action", "Undo"),
             IconCode::Code::UNDO
             ),
    UiAction("action://redo",
             { "action://trackedit/redo" },
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Redo"),
             TranslatableString("action", "Redo"),
             IconCode::Code::REDO
             ),
    UiAction("action://delete",
             { "action://trackedit/delete" },
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "De&lete"),
             TranslatableString("action", "Delete"),
             IconCode::Code::DELETE_TANK
             ),
    UiAction("action://cancel",
             au::context::UiCtxAny,
             au::context::CTX_DISABLED,
             TranslatableString("action", "&Cancel"),
             TranslatableString("action", "Cancel"),
             IconCode::Code::DELETE_TANK
             ),
    UiAction("action://trigger",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Trigger"),
             TranslatableString("action", "Trigger")
             ),
};

ApplicationUiActions::ApplicationUiActions(const muse::modularity::ContextPtr& ctx, std::shared_ptr<ApplicationActionController> controller)
    : muse::Contextable(ctx), m_controller(controller)
{
}

void ApplicationUiActions::init()
{
    mainWindow()->isFullScreenChanged().onNotify(this, [this]() {
        m_actionCheckedChanged.send({ FULL_SCREEN_CODE });
    });

    dockWindowProvider()->windowChanged().onNotify(this, [this]() {
        listenOpenedDocksChanged(dockWindowProvider()->window());
    });

    recordController()->isRecordingChanged().onNotify(this, [this]() {
        m_actionEnabledChanged.send(m_controller->prohibitedActionsWhileRecording());
    });
}

void ApplicationUiActions::listenOpenedDocksChanged(IDockWindow* window)
{
    if (!window) {
        return;
    }

    window->docksOpenStatusChanged().onReceive(this, [this](const QStringList& dockNames) {
        ActionCodeList actions;

        for (const ActionCode& toggleDockAction : toggleDockActions().keys()) {
            const DockName& dockName = toggleDockActions()[toggleDockAction];

            if (dockNames.contains(dockName)) {
                actions.push_back(toggleDockAction);
            }
        }

        if (!actions.empty()) {
            m_actionCheckedChanged.send(actions);
        }
    });
}

const muse::ui::UiActionList& ApplicationUiActions::actionsList() const
{
    return m_actions;
}

bool ApplicationUiActions::actionEnabled(const UiAction& act) const
{
    if (!m_controller) {
        return true;
    }
    if (!m_controller->canReceiveAction(act.code)) {
        return false;
    }

    return true;
}

bool ApplicationUiActions::actionChecked(const UiAction& act) const
{
    if (act.code == FULL_SCREEN_CODE) {
        return mainWindow()->isFullScreen();
    }

    QMap<ActionCode, DockName> toggleDockActions = ApplicationUiActions::toggleDockActions();
    DockName dockName = toggleDockActions.value(act.code, DockName());

    if (dockName.isEmpty()) {
        return false;
    }

    const IDockWindow* window = dockWindowProvider()->window();
    return window ? window->isDockOpen(dockName) : false;
}

muse::async::Channel<muse::actions::ActionCodeList> ApplicationUiActions::actionEnabledChanged() const
{
    return m_actionEnabledChanged;
}

muse::async::Channel<muse::actions::ActionCodeList> ApplicationUiActions::actionCheckedChanged() const
{
    return m_actionCheckedChanged;
}

const QMap<muse::actions::ActionCode, DockName>& ApplicationUiActions::toggleDockActions()
{
    static const QMap<muse::actions::ActionCode, DockName> actionsMap {
        { "toggle-transport", PLAYBACK_TOOLBAR_NAME },

        { "toggle-tracks", TRACKS_PANEL_NAME },
        { "toggle-history", HISTORY_PANEL_NAME },

        { "toggle-statusbar", PROJECT_STATUSBAR_NAME },
    };

    return actionsMap;
}
