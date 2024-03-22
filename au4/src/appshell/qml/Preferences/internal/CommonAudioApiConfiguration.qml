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

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0
import MuseScore.Preferences 1.0

Item {
    id: root

    property int columnWidth: 0

    property NavigationPanel navigation: null
    property int navigationOrderStart: 0

    width: parent.width
    height: content.height

    CommonAudioApiConfigurationModel {
        id: apiModel
    }

    Component.onCompleted: {
        apiModel.load()
    }

    Column {
        id: content

        spacing: 12

        ComboBoxWithTitle {
            title: qsTrc("appshell/preferences", "Audio device:")
            columnWidth: root.columnWidth

            currentIndex: indexOfValue(apiModel.currentDeviceId)
            model: apiModel.deviceList

            navigation.name: "AudioDeviceBox"
            navigation.panel: root.navigation
            navigation.row: root.navigationOrderStart

            onValueEdited: function(newIndex, newValue) {
                apiModel.deviceSelected(newValue)
            }
        }

        ComboBoxWithTitle {
            id: bufferSize

            title: qsTrc("appshell", "Buffer size:")
            columnWidth: root.columnWidth

            currentIndex: indexOfValue(apiModel.bufferSize)
            model: apiModel.bufferSizeList

            navigation.name: "BufferSizeBox"
            navigation.panel: root.navigation
            navigation.row: root.navigationOrderStart + 1

            onValueEdited: function(newIndex, newValue) {
                apiModel.bufferSizeSelected(newValue)
            }
        }
    }
}
