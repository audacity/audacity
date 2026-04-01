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

import Muse.UiComponents

import Audacity.UiComponents 1.0
import Audacity.Playback 1.0

BaseSection {
    id: root

    title: qsTrc("preferences", "Inputs and outputs")
    spacing: 16

    property int currentAudioApiIndex: -1
    property var audioApiList: null
    property var apiModel: null

    signal currentAudioApiIndexChangeRequested(int newIndex)

    PlaybackStateModel {
        id: playbackState
    }

    Component.onCompleted: {
        playbackState.init()
    }

    Row {
        width: parent.width
        spacing: root.spacing

        Column {
            width: root.columnWidth
            spacing: root.spacing

            ComboBoxWithTitle {
                title: qsTrc("preferences", "Host")
                columnWidth: root.columnWidth

                enabled: !playbackState.isPlaying

                currentIndex: apiModel.currentAudioApiIndex
                model: root.audioApiList

                navigation.name: "AudioApiBox"
                navigation.panel: root.navigation
                navigation.row: 1

                onValueEdited: function(newIndex, newValue) {
                    root.currentAudioApiIndexChangeRequested(newIndex)
                }
            }

            ComboBoxWithTitle {
                title: qsTrc("preferences", "Recording device")
                columnWidth: root.columnWidth

                enabled: !playbackState.isPlaying

                currentIndex: indexOfValue(apiModel.currentInputDeviceId)
                model: apiModel.inputDeviceList

                navigation.name: "RecordingDeviceBox"
                navigation.panel: root.navigation
                navigation.row: 3

                onValueEdited: function(newIndex, newValue) {
                    apiModel.inputDeviceSelected(newValue)
                }
            }
        }

        Column {
            width: root.columnWidth
            spacing: root.spacing

            ComboBoxWithTitle {
                title: qsTrc("preferences", "Playback device")
                columnWidth: root.columnWidth

                enabled: !playbackState.isPlaying

                currentIndex: indexOfValue(apiModel.currentOutputDeviceId)
                model: apiModel.outputDeviceList

                navigation.name: "PlaybackDeviceBox"
                navigation.panel: root.navigation
                navigation.row: 2

                onValueEdited: function(newIndex, newValue) {
                    apiModel.outputDeviceSelected(newValue)
                }
            }

            ComboBoxWithTitle {
                title: qsTrc("preferences", "Recording channels")
                columnWidth: root.columnWidth

                enabled: !playbackState.isPlaying

                currentIndex: indexOfValue(apiModel.currentInputChannelsSelected)
                model: apiModel.inputChannelsList

                navigation.name: "RecordingChannelsBox"
                navigation.panel: root.navigation
                navigation.row: 4

                onValueEdited: function(newIndex, newValue) {
                    apiModel.inputChannelsSelected(newIndex)
                }
            }
        }
    }
}
