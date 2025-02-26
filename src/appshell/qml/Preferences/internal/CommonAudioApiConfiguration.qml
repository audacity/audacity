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
import Audacity.Preferences 1.0

Item {
    id: root

    property int columnWidth: 208

    property NavigationPanel navigation: null
    property int navigationOrderStart: 0

    property var apiModel: null

    width: parent.width
    height: content.height

    Column {
        id: content

        spacing: 16

        ComboBoxWithTitle {
            title: qsTrc("appshell/preferences", "Playback device")
            columnWidth: root.columnWidth

            currentIndex: indexOfValue(apiModel.currentOutputDeviceId)
            model: apiModel.outputDeviceList

            navigation.name: "PlaybackDeviceBox"
            navigation.panel: root.navigation
            navigation.row: root.navigationOrderStart

            onValueEdited: function(newIndex, newValue) {
                apiModel.outputDeviceSelected(newValue)
            }
        }

        ComboBoxWithTitle {
            title: qsTrc("appshell/preferences", "Recording device")
            columnWidth: root.columnWidth

            currentIndex: indexOfValue(apiModel.currentInputDeviceId)
            model: apiModel.inputDeviceList

            navigation.name: "RecordingDeviceBox"
            navigation.panel: root.navigation
            navigation.row: root.navigationOrderStart + 1

            onValueEdited: function(newIndex, newValue) {
                apiModel.inputDeviceSelected(newValue)
            }
        }

        ComboBoxWithTitle {
            title: qsTrc("appshell/preferences", "Recording channels")
            columnWidth: root.columnWidth

            currentIndex: indexOfValue(apiModel.currentInputChannels)
            model: apiModel.inputChannelsList

            navigation.name: "RecordingChannelsBox"
            navigation.panel: root.navigation
            navigation.row: root.navigationOrderStart + 2

            onValueEdited: function(newIndex, newValue) {
                apiModel.inputChannelsSelected(newValue)
            }
        }
    }
}
