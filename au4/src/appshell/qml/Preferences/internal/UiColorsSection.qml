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
import Audacity.Preferences 1.0

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "UI colors")
    navigation.direction: NavigationPanel.Both

    signal colorChangeRequested(var newColor, var propertyType)

    GridLayout {
        id: grid
        width: parent.width

        columnSpacing: root.columnSpacing
        rowSpacing: root.rowSpacing
        columns: 2

        Repeater {
            model: [
                { textRole: qsTrc("appshell/preferences", "Accent color:"), colorRole: ui.theme.accentColor, typeRole: AppearancePreferencesModel.AccentColor},
                { textRole: qsTrc("appshell/preferences", "Text and icons:"), colorRole: ui.theme.fontPrimaryColor, typeRole: AppearancePreferencesModel.TextAndIconsColor},
                { textRole: qsTrc("appshell/preferences", "Disabled text:"), colorRole: "#000000", typeRole: AppearancePreferencesModel.DisabledColor},
                { textRole: qsTrc("appshell/preferences", "Border color:"), colorRole: ui.theme.strokeColor, typeRole: AppearancePreferencesModel.BorderColor}
            ]

            delegate: Row {
                Layout.preferredWidth: (grid.width - grid.columnSpacing) / 2
                spacing: root.columnSpacing

                StyledTextLabel {
                    id: titleLabel
                    anchors.verticalCenter: parent.verticalCenter
                    text: modelData["textRole"]
                    width: root.columnWidth / 2
                    horizontalAlignment: Text.AlignLeft
                }

                ColorPicker {
                    width: 112
                    color: modelData["colorRole"]

                    navigation.name: titleLabel.text
                    navigation.panel: root.navigation
                    navigation.row: index / grid.columns
                    navigation.column: index % grid.columns
                    navigation.accessible.name: titleLabel.text

                    onNewColorSelected: function(newColor) {
                        root.colorChangeRequested(newColor, modelData.typeRole)
                    }
                }
            }
        }
    }
}
