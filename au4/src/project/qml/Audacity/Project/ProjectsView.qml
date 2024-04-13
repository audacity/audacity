/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
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
import QtQuick 2.15

import Muse.UiComponents 1.0
import Muse.Ui 1.0
import Audacity.Project 1.0

Loader {
    id: root

    property string searchText

    property int viewType: ProjectsPageModel.Grid

    property color backgroundColor: ui.theme.backgroundSecondaryColor
    property real sideMargin: 46

    property NavigationSection navigationSection
    property int navigationOrder

    signal createNewProjectRequested()
    signal openProjectRequested(var projectPath, var displayName)
}
