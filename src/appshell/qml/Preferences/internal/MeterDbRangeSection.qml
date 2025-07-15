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

    title: qsTrc("appshell/preferences", "Meter dB range")
    spacing: 16

    property var model: null

    PlaybackStateModel {
        id: playbackState
    }

    ComboBoxWithTitle {
        title: qsTrc("appshell/preferences", "dB range")

        columnWidth: 320

        currentIndex: root.model.meterDbRange
        model: [
            "-36 dB (shallow range for high-amplitude editing)",
            "-48 dB (PCM range of 8 bit samples)", 
            "-60 dB (PCM range of 10 bit samples)",
            "-72 dB (PCM range of 12 bit samples)",
            "-84 dB (PCM range of 14 bit samples)",
            "-96 dB (PCM range of 16 bit samples)",
            "-120 dB (approximate limit of human hearing)",
            "-145 dB (PCM range of 24 bit samples)"
        ]
        
        navigation.name: "MeterDbRangeBox"
        navigation.panel: root.navigation
        navigation.row: 1

        onValueEdited: function(newIndex, newValue) {
            root.model.meterDbRange = newIndex;
        }
    }
}
