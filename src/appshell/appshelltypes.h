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

#ifndef AU_APPSHELL_APPSHELLTYPES_H
#define AU_APPSHELL_APPSHELLTYPES_H

#include <QString>

namespace au::appshell {
using DockName = QString;

// Panels:
static const DockName TRACKS_PANEL_NAME("tracksPanel");
static const DockName HISTORY_PANEL_NAME("historyPanel");
static const DockName PLAYBACK_METER_PANEL_NAME("playbackMeterPanel");

// Toolbars:
static const DockName PROJECT_TOOLBAR_NAME("projectToolBar");
static const DockName UNDO_REDO_TOOLBAR_NAME("undoRedoToolBar");
static const DockName PLAYBACK_TOOLBAR_NAME("playbackToolBar");
static const DockName WORKSPACES_TOOLBAR_NAME("workspacesToolBar");

// Other:
static const DockName PROJECT_STATUSBAR_NAME("projectStatusBar");

enum class StartupModeType
{
    StartEmpty,
    ContinueLastSession,
    StartWithNewProject,
    StartWithProject,
    Recovery,
    FirstLaunch
};

enum class FactoryResetMode {
    Full,         // Clean user data files + reset settings
    SettingsOnly  // Reset settings only, keep user data files
};
}

#endif // AU_APPSHELL_APPSHELLTYPES_H
