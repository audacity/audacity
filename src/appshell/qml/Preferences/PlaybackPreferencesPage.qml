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
import Audacity.Preferences 1.0

import "internal"

PreferencesPage {
    id: root

    PlaybackPreferencesModel {
        id: playbackModel
    }

    Component.onCompleted: {
        playbackModel.init()
    }

    Column {
        width: parent.width
        spacing: root.sectionsSpacing

        AudioApiSection {
            currentAudioApiIndex: playbackModel.currentAudioApiIndex
            audioApiList: playbackModel.audioApiList()

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 1

            onCurrentAudioApiIndexChangeRequested: function(newIndex) {
                playbackModel.currentAudioApiIndex = newIndex
            }
        }

        SeparatorLine {}

        // MidiDevicesSection {
        //     inputDeviceId: playbackModel.midiInputDeviceId
        //     outputDeviceId: playbackModel.midiOutputDeviceId
        //     inputDevices: playbackModel.midiInputDevices
        //     outputDevices: playbackModel.midiOutputDevices

        //     isMIDI20OutputSupported: playbackModel.isMIDI20OutputSupported
        //     useMIDI20Output: playbackModel.useMIDI20Output

        //     navigation.section: root.navigationSection
        //     navigation.order: root.navigationOrderStart + 2

        //     onInputDeviceIdChangeRequested: function(newId) {
        //         playbackModel.inputDeviceSelected(newId)
        //     }

        //     onOutputDeviceIdChangeRequested: function(newId) {
        //         playbackModel.outputDeviceSelected(newId)
        //     }

        //     onUseMIDI20OutputChangeRequested: function(use) {
        //         playbackModel.useMIDI20Output = use
        //     }
        // }

        // SeparatorLine {}

        // MixerSection {
        //     muteHiddenInstruments: playbackModel.muteHiddenInstruments

        //     navigation.section: root.navigationSection
        //     navigation.order: root.navigationOrderStart + 3

        //     onMuteHiddenInstrumentsChangeRequested: function(mute) {
        //         playbackModel.muteHiddenInstruments = mute
        //     }
        // }

        /*
         * TODO: https://github.com/musescore/MuseScore/issues/9807
        SeparatorLine {}

        AudioEngineSection {
            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 3

            onRestartAudioAndMidiDevicesRequested: {
                ioModel.restartAudioAndMidiDevices()
            }
        }
         */
    }
}
