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
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

BaseSection {
    id: root

    navigation.direction: NavigationPanel.Both

    property alias wallpaperDialogTitle: wallpaperPicker.dialogTitle

    property bool useColor: true
    property alias color: colorPicker.color

    property alias wallpaperPath: wallpaperPicker.path
    property alias wallpapersDir: wallpaperPicker.dir
    property alias wallpaperFilter: wallpaperPicker.filter

    property alias opacityOverride: gridSection.opacity

    signal useColorChangeRequested(var newValue)
    signal colorChangeRequested(var newColor)
    signal wallpaperPathChangeRequested(var newWallpaperPath)

    GridLayout {
        id: gridSection

        rows: 2
        columns: 2

        rowSpacing: root.rowSpacing
        columnSpacing: root.columnSpacing

        RoundedRadioButton {
            id: colorButton
            implicitWidth: root.columnWidth

            checked: root.useColor
            text: qsTrc("appshell/preferences", "Color:")

            navigation.name: "ColorBox"
            navigation.panel: root.navigation
            navigation.row: 0
            navigation.column: 0

            onToggled: {
                root.useColorChangeRequested(true)
            }
        }

        ColorPicker {
            id: colorPicker

            width: 112

            enabled: root.useColor

            navigation.name: "ColorBox"
            navigation.panel: root.navigation
            navigation.row: 0
            navigation.column: 1

            onNewColorSelected: function(newColor) {
                root.colorChangeRequested(newColor)
            }
        }

        RoundedRadioButton {
            implicitWidth: root.columnWidth

            checked: !root.useColor
            text: qsTrc("appshell/preferences", "Wallpaper:")

            navigation.name: "WallpaperBox"
            navigation.panel: root.navigation
            navigation.row: 1
            navigation.column: 0

            onToggled: {
                root.useColorChangeRequested(false)
            }
        }

        FilePicker {
            id: wallpaperPicker
            pathFieldWidth: root.columnWidth

            enabled: !root.useColor

            navigation: root.navigation
            navigationRowOrderStart: 1
            navigationColumnOrderStart: 1

            onPathEdited: function(newPath) {
                root.wallpaperPathChangeRequested(newPath)
            }
        }
    }
}
