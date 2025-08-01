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

import Muse.UiComponents 1.0
import Audacity.Playback 1.0
import Audacity.Preferences

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Sample rate")
    spacing: 16

    property var apiModel: null

    PlaybackStateModel {
        id: playbackState
    }

    Row {
        spacing: 8

        ComboBoxWithTitle {
            title: qsTrc("appshell/preferences", "Default sample rate")
            columnWidth: root.columnWidth

            enabled: !playbackState.isPlaying

            currentIndex: indexOfValue(apiModel.defaultSampleRate)
            model: apiModel.defaultSampleRateList

            navigation.name: "DefaultSampleRateBox"
            navigation.panel: root.navigation
            navigation.row: 1
            navigation.column: 0

            onValueEdited: function(newIndex, newValue) {
                apiModel.defaultSampleRateSelected(newValue)
            }
        }

        IncrementalPropertyControlWithTitle {
            currentValue: apiModel.defaultSampleRateValue

            enabled: !playbackState.isPlaying
            visible: apiModel.otherSampleRate

            minValue: 1
            maxValue: 999999

            measureUnitsSymbol: qsTrc("global", "Hz")

            navigation.name: "SampleRateControl"
            navigation.panel: root.navigation
            navigation.row: 1
            navigation.column: 1

            onValueEdited: function(newValue) {
                apiModel.defaultSampleRateValueSelected(newValue)
            }
        }
    }

    ComboBoxWithTitle {
        title: qsTrc("appshell/preferences", "Default sample format")
        columnWidth: root.columnWidth

        enabled: !playbackState.isPlaying

        currentIndex: indexOfValue(apiModel.defaultSampleFormat)
        model: apiModel.defaultSampleFormatList

        navigation.name: "DefaultSampleFormatBox"
        navigation.panel: root.navigation
        navigation.row: 2

        onValueEdited: function(newIndex, newValue) {
            apiModel.defaultSampleFormatSelected(newValue)
        }
    }

    StyledTextLabel {
        text: "Default sample rates and formats apply to newly created tracks only. Recording into existing tracks will use the track’s sample rate and format instead."

        width: root.width

        horizontalAlignment: Text.AlignLeft

        wrapMode: Text.WordWrap

    }
}
