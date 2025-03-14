/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-Studio-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
 *
 * Copyright (C) 2025 Audacity Limited
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

import Audacity.Playback

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Solo button behavior")

    navigation.direction: NavigationPanel.Vertical

    property var playbackModel: null

    columnSpacing: 16

    RadioButtonGroup {
        id: soloBtnGroup

        spacing: root.rowSpacing
        orientation: Qt.Vertical

        width: parent.width
        height: 55

        Column {
            width: parent.width
            spacing: root.columnSpacing

            RoundedRadioButton {
                width: parent.width

                checked: playbackModel.soloBehavior == SoloBehavior.SoloBehaviorMulti
                text: qsTrc("appshell/preferences", "Solo can be activated for multiple tracks at the same time")

                navigation.name: "SoloBehaviorMultiBox"
                navigation.panel: root.navigation
                navigation.row: 0

                onToggled: {
                    playbackModel.soloBehaviorSelected(SoloBehavior.SoloBehaviorMulti)
                }
            }

            RoundedRadioButton {
                width: parent.width

                checked: playbackModel.soloBehavior == SoloBehavior.SoloBehaviorSimple
                text: qsTrc("appshell/preferences", "When solo is activated, it deactivates solo for all other tracks")

                navigation.name: "SoloBehaviorSimpleBox"
                navigation.panel: root.navigation
                navigation.row: 1

                onToggled: {
                    playbackModel.soloBehaviorSelected(SoloBehavior.SoloBehaviorSimple)
                }
            }
        }
    }
}
