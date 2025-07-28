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

import Muse.UiComponents 1.0
import Audacity.Playback 1.0

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Inputs and outputs")
    spacing: 16

    property int currentAudioApiIndex: -1
    property var audioApiList: null
    property var apiModel: null

    signal currentAudioApiIndexChangeRequested(int newIndex)

    PlaybackStateModel {
        id: playbackState
    }

    // /*
    //  * TODO: https://github.com/musescore/MuseScore/issues/9807
    ComboBoxWithTitle {
        id: apiComboBox

        title: qsTrc("appshell/preferences", "Host")
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
    // */

    CommonAudioApiConfiguration {
        columnWidth: Math.max(apiModel.longestDeviceNameLength, root.columnWidth)

        enabled: !playbackState.isPlaying

        navigation: root.navigation
        navigationOrderStart: 2

        apiModel: root.apiModel
    }
}
