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
#include "notationpagemodel.h"

#include "internal/applicationuiactions.h"
#include "dockwindow/idockwindow.h"

#include "log.h"

using namespace mu::appshell;
//using namespace mu::notation;
using namespace mu::actions;

NotationPageModel::NotationPageModel(QObject* parent)
    : QObject(parent)
{
}

bool NotationPageModel::isNavigatorVisible() const
{
    return configuration()->isNotationNavigatorVisible();
}

bool NotationPageModel::isBraillePanelVisible() const
{
    return false;
    //return brailleConfiguration()->braillePanelEnabled();
}

void NotationPageModel::init()
{
    TRACEFUNC;

    for (const ActionCode& actionCode : ApplicationUiActions::toggleDockActions().keys()) {
        DockName dockName = ApplicationUiActions::toggleDockActions()[actionCode];
        dispatcher()->reg(this, actionCode, [=]() { toggleDock(dockName); });
    }

    // globalContext()->currentNotationChanged().onNotify(this, [this]() {
    //     onNotationChanged();
    // });

    // brailleConfiguration()->braillePanelEnabledChanged().onNotify(this, [this]() {
    //     emit isBraillePanelVisibleChanged();
    // });

    onNotationChanged();
    updateDrumsetPanelVisibility();
}

QString NotationPageModel::notationToolBarName() const
{
    return NOTATION_TOOLBAR_NAME;
}

QString NotationPageModel::playbackToolBarName() const
{
    return PLAYBACK_TOOLBAR_NAME;
}

QString NotationPageModel::undoRedoToolBarName() const
{
    return UNDO_REDO_TOOLBAR_NAME;
}

QString NotationPageModel::noteInputBarName() const
{
    return NOTE_INPUT_BAR_NAME;
}

QString NotationPageModel::palettesPanelName() const
{
    return PALETTES_PANEL_NAME;
}

QString NotationPageModel::instrumentsPanelName() const
{
    return INSTRUMENTS_PANEL_NAME;
}

QString NotationPageModel::inspectorPanelName() const
{
    return INSPECTOR_PANEL_NAME;
}

QString NotationPageModel::selectionFiltersPanelName() const
{
    return SELECTION_FILTERS_PANEL_NAME;
}

QString NotationPageModel::mixerPanelName() const
{
    return MIXER_PANEL_NAME;
}

QString NotationPageModel::pianoKeyboardPanelName() const
{
    return PIANO_KEYBOARD_PANEL_NAME;
}

QString NotationPageModel::timelinePanelName() const
{
    return TIMELINE_PANEL_NAME;
}

QString NotationPageModel::drumsetPanelName() const
{
    return DRUMSET_PANEL_NAME;
}

QString NotationPageModel::statusBarName() const
{
    return NOTATION_STATUSBAR_NAME;
}

void NotationPageModel::onNotationChanged()
{
    // INotationPtr notation = globalContext()->currentNotation();
    // if (!notation) {
    //     return;
    // }

    // INotationNoteInputPtr noteInput = notation->interaction()->noteInput();
    // noteInput->stateChanged().onNotify(this, [this]() {
    //     updateDrumsetPanelVisibility();
    // });
}

void NotationPageModel::toggleDock(const QString& name)
{
    if (name == NOTATION_NAVIGATOR_PANEL_NAME) {
        configuration()->setIsNotationNavigatorVisible(!isNavigatorVisible());
        emit isNavigatorVisibleChanged();
        return;
    }

    // if (name == NOTATION_BRAILLE_PANEL_NAME) {
    //     brailleConfiguration()->setBraillePanelEnabled(!isBraillePanelVisible());
    //     emit isBraillePanelVisibleChanged();
    //     return;
    // }

    dispatcher()->dispatch("dock-toggle", ActionData::make_arg1<QString>(name));
}

void NotationPageModel::updateDrumsetPanelVisibility()
{
    TRACEFUNC;

    // const dock::IDockWindow* window = dockWindowProvider()->window();
    // if (!window) {
    //     return;
    // }

    // auto setDrumsetPanelOpen = [this, window](bool open) {
    //     if (open == window->isDockOpen(DRUMSET_PANEL_NAME)) {
    //         return;
    //     }

    //     dispatcher()->dispatch("dock-set-open", ActionData::make_arg2<QString, bool>(DRUMSET_PANEL_NAME, open));
    // };

    // const INotationPtr notation = globalContext()->currentNotation();
    // if (!notation) {
    //     setDrumsetPanelOpen(false);
    //     return;
    // }

    // const INotationNoteInputPtr noteInput = notation->interaction()->noteInput();
    // bool isNeedOpen = noteInput->isNoteInputMode() && noteInput->state().drumset != nullptr;

    // setDrumsetPanelOpen(isNeedOpen);
}
