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

    title: qsTrc("appshell/preferences", "Playback performance")
    spacing: 16

    property var playbackModel: null

    PlaybackStateModel {
        id: playbackState
    }

    ComboBoxWithTitle {
        title: qsTrc("appshell/preferences", "Playback quality")
        columnWidth: root.columnWidth

        enabled: playbackState.isPlaying()

        currentIndex: indexOfValue(playbackModel.currentPlaybackQuality)
        model: playbackModel.playbackQualityList

        navigation.name: "DefaultSampleRateBox"
        navigation.panel: root.navigation
        navigation.row: 0

        onValueEdited: function(newIndex, newValue) {
            playbackModel.playbackQualitySelected(newValue)
        }
    }

    ComboBoxWithTitle {
        title: qsTrc("appshell/preferences", "Dithering")
        columnWidth: root.columnWidth

        enabled: !(playbackState.isPaused() || playbackState.isPlaying())

        currentIndex: indexOfValue(playbackModel.currentDithering)
        model: playbackModel.ditheringList

        navigation.name: "DefaultSampleFormatBox"
        navigation.panel: root.navigation
        navigation.row: 1

        onValueEdited: function(newIndex, newValue) {
            playbackModel.ditheringSelected(newValue)
        }
    }
}
