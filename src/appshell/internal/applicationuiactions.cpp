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

using namespace mu;
using namespace au::appshell;

using namespace muse;
using namespace muse::ui;
using namespace muse::actions;
using namespace muse::dock;

static const ActionCode FULL_SCREEN_CODE("fullscreen");
static const ActionCode TOGGLE_NAVIGATOR_ACTION_CODE("toggle-navigator");
static const ActionCode TOGGLE_BRAILLE_ACTION_CODE("toggle-braille-panel");

const UiActionList ApplicationUiActions::m_actions = {
    UiAction("quit",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Exit"),
             TranslatableString("action", "Exit")
             ),
    UiAction("restart",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Restart")
             ),
    UiAction(FULL_SCREEN_CODE,
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&Full screen"),
             TranslatableString("action", "Full screen"),
             Checkable::Yes
             ),
    UiAction("about-musescore",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "&About MuseScore…")
             ),
    UiAction("about-qt",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "About &Qt…")
             ),
    UiAction("about-musicxml",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "About &MusicXML…")
             ),
    UiAction("online-handbook",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
             TranslatableString("action", "Online &handbook"),
             TranslatableString("action", "Open online handbook")
             ),
    UiAction("ask-help",
             au::context::UiCtxAny,
             au::context::CTX_ANY,
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
             au::context::CTX_ANY,
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
    UiAction("toggle-noteinput",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "&Note input"),
             TranslatableString("action", "Show/hide note input toolbar"),
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
    UiAction("toggle-instruments",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Instr&uments"),
             TranslatableString("action", "Open instruments dialog…"),
             Checkable::Yes
             ),
    UiAction("inspector",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Propert&ies"),
             TranslatableString("action", "Show/hide properties"),
             Checkable::Yes
             ),
    UiAction("toggle-selection-filter",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             TranslatableString("action", "Se&lection filter"),
             TranslatableString("action", "Show/hide selection filter"),
             Checkable::Yes
             ),

    // Navigator
    UiAction(TOGGLE_NAVIGATOR_ACTION_CODE,
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "&Navigator"),
             TranslatableString("action", "Show/hide navigator"),
             Checkable::Yes
             ),

    // Braille panel
    UiAction(TOGGLE_BRAILLE_ACTION_CODE,
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "&Braille"),
             TranslatableString("action", "Show/hide braille panel"),
             Checkable::Yes
             ),

    // Horizontal panels
    UiAction("toggle-timeline",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Tim&eline"),
             TranslatableString("action", "Show/hide timeline"),
             Checkable::Yes
             ),
    UiAction("toggle-mixer",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Mixer"),
             TranslatableString("action", "Show/hide mixer"),
             IconCode::Code::MIXER,
             Checkable::Yes
             ),
    UiAction("toggle-piano-keyboard",
             au::context::UiCtxProjectOpened,
             au::context::CTX_ANY,
             TranslatableString("action", "Piano &keyboard"),
             TranslatableString("action", "Show/hide piano keyboard"),
             Checkable::Yes
             ),
    UiAction("toggle-scorecmp-tool",
             au::context::UiCtxProjectOpened,
             au::context::CTX_PROJECT_OPENED,
             TranslatableString("action", "Score comparison tool"),
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
             )
};

ApplicationUiActions::ApplicationUiActions(std::shared_ptr<ApplicationActionController> controller)
    : m_controller(controller)
{
}

void ApplicationUiActions::init()
{
    mainWindow()->isFullScreenChanged().onNotify(this, [this]() {
        m_actionCheckedChanged.send({ FULL_SCREEN_CODE });
    });

    configuration()->isNotationNavigatorVisibleChanged().onNotify(this, [this]() {
        m_actionCheckedChanged.send({ TOGGLE_NAVIGATOR_ACTION_CODE });
    });

    //! TODO AU4
    // brailleConfiguration()->braillePanelEnabledChanged().onNotify(this, [this]() {
    //     m_actionCheckedChanged.send({ TOGGLE_BRAILLE_ACTION_CODE });
    // });

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

    if (dockName == NOTATION_NAVIGATOR_PANEL_NAME) {
        return configuration()->isNotationNavigatorVisible();
    }

    //! TODO AU4
    // if (dockName == NOTATION_BRAILLE_PANEL_NAME) {
    //     return brailleConfiguration()->braillePanelEnabled();
    // }

    const IDockWindow* window = dockWindowProvider()->window();
    return window ? window->isDockOpenAndCurrentInFrame(dockName) : false;
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
        { "toggle-noteinput", NOTE_INPUT_BAR_NAME },

        { "toggle-tracks", TRACKS_PANEL_NAME },
        { "toggle-instruments", INSTRUMENTS_PANEL_NAME },
        { "inspector", INSPECTOR_PANEL_NAME },
        { "toggle-selection-filter", SELECTION_FILTERS_PANEL_NAME },

        { TOGGLE_NAVIGATOR_ACTION_CODE, NOTATION_NAVIGATOR_PANEL_NAME },
        { TOGGLE_BRAILLE_ACTION_CODE, NOTATION_BRAILLE_PANEL_NAME },

        { "toggle-timeline", TIMELINE_PANEL_NAME },
        { "toggle-mixer", MIXER_PANEL_NAME },
        { "toggle-piano-keyboard", PIANO_KEYBOARD_PANEL_NAME },

        { "toggle-statusbar", PROJECT_STATUSBAR_NAME },
    };

    return actionsMap;
}
