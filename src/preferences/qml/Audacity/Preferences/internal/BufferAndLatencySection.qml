/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
 *
 * Copyright (C) 2025 Audacity BVBA and others
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

import Muse.UiComponents

import Audacity.UiComponents 1.0
import Audacity.Playback 1.0
import Audacity.AppShell

BaseSection {
    id: root

    title: qsTrc("preferences", "Buffer and latency")
    spacing: 16

    property var apiModel: null

    PlaybackStateModel {
        id: playbackState
    }

    Component.onCompleted: {
        playbackState.init()
    }

    IncrementalPropertyControlWithTitle {
        title: qsTrc("preferences", "Buffer length")

        currentValue: apiModel.bufferLength

        enabled: !playbackState.isPlaying

        columnWidth: root.columnWidth
        spacing: root.columnSpacing

        //: Abbreviation of "milliseconds"
        measureUnitsSymbol: qsTrc("global", "ms")

        navigation.name: "BufferLengthControl"
        navigation.panel: root.navigation
        navigation.row: 1

        onValueEdited: function (newValue) {
            apiModel.bufferLengthSelected(newValue)
        }
    }

    StyledTextLabel {
        text: qsTrc("preferences", "Latency compensation")

        width: root.columnWidth
        horizontalAlignment: Qt.AlignLeft
        wrapMode: Text.WordWrap
        maximumLineCount: 2
    }

    CheckBox {
        id: automaticCompensationCheckbox

        text: qsTrc("preferences", "Automatic")
        checked: apiModel.automaticCompensationEnabled
        onClicked: apiModel.setAutomaticCompensationEnabled(!checked)

        navigation.name: "AutomaticLatencyCompensationCheckBox"
        navigation.panel: root.navigation
        navigation.row: 2
    }

    IncrementalPropertyControl {

        currentValue: apiModel.latencyCompensation

        enabled: !playbackState.isPlaying && !apiModel.automaticCompensationEnabled
        implicitWidth: 100

        //: Abbreviation of "milliseconds"
        measureUnitsSymbol: qsTrc("global", "ms")

        navigation.name: "LatencyCompensationControl"
        navigation.panel: root.navigation
        navigation.row: 3

        onValueEdited: function (newValue) {
            apiModel.latencyCompensationSelected(newValue)
        }
    }
}
