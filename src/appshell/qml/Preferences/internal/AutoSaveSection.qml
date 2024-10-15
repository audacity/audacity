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

import Muse.Ui 1.0
import Muse.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Auto save")

    navigation.direction: NavigationPanel.Horizontal

    property alias isAutoSaveEnabled: autoSaveCheckBox.checked
    property alias autoSaveInterval: autoSaveIntervalControl.currentValue

    signal autoSaveEnabledChanged(bool enabled)
    signal intervalChanged(int minutes)

    Row {
        spacing: 12

        CheckBox {
            id: autoSaveCheckBox

            width: root.columnWidth
            anchors.verticalCenter: parent.verticalCenter

            text: qsTrc("appshell/preferences", "Auto save every:")

            navigation.name: "AutoSaveCheckBox"
            navigation.panel: root.navigation
            navigation.column: 1

            onClicked: {
                root.autoSaveEnabledChanged(!checked)
            }
        }

        IncrementalPropertyControl {
            id: autoSaveIntervalControl

            width: 96
            anchors.verticalCenter: parent.verticalCenter

            enabled: root.isAutoSaveEnabled

            minValue: 1
            maxValue: 100
            step: 1
            decimals: 0

            measureUnitsSymbol: " " + qsTrc("global", "min", /*disambiguation*/ "abbreviation of minutes")

            navigation.name: "AutoSavePeriodControl"
            navigation.panel: root.navigation
            navigation.column: 2

            onValueEdited: function(newValue) {
                root.intervalChanged(newValue)
            }
        }
    }
}
