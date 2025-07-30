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
import Audacity.Preferences 1.0
import Audacity.Playback 1.0

import "internal"

PreferencesPage {
    id: root

    property int navigationOrderStart: 0

    CommonAudioApiConfigurationModel {
        id: apiModel
    }

    Component.onCompleted: {
        apiModel.load()
    }

    Column {
        width: parent.width
        spacing: root.sectionsSpacing

        AudioApiSection {
            currentAudioApiIndex: apiModel.currentAudioApiIndex
            audioApiList: apiModel.audioApiList()
            apiModel: apiModel

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart

            onCurrentAudioApiIndexChangeRequested: function(newIndex) {
                apiModel.currentAudioApiIndex = newIndex
            }
        }

        SeparatorLine {}

        BufferAndLatencySection {
            apiModel: apiModel

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 1
        }

        SeparatorLine {}

        SampleRateSection {
            apiModel: apiModel

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 2
        }

        SeparatorLine {}

        MeterDbRangeSection {
           navigation.section: root.navigationSection
           navigation.order: root.navigationOrderStart + 3
        }
    }
}
