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

import "internal"

ListView {
    id: root

    property alias themes: root.model
    property string currentThemeCode

    currentIndex: model.findIndex((theme) => theme.codeKey === currentThemeCode)

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "ThemeSamplesList"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal

        onNavigationEvent: function(event) {
            if (event.type === NavigationEvent.AboutActive) {
                event.setData("controlIndex", [navigationRow, navigationColumnStart + root.currentIndex])
            }
        }
    }

    property int navigationRow: -1
    property int navigationColumnStart: 0
    readonly property int navigationColumnEnd: navigationColumnStart + count

    signal themeChangeRequested(var newThemeCode)

    readonly property int sampleWidth: 112
    readonly property int sampleHeight: 120

    implicitWidth: count * sampleWidth + (count - 1) * spacing
    height: contentHeight
    contentHeight: sampleHeight

    orientation: Qt.Horizontal
    interactive: false

    spacing: 64

    delegate: Column {
        width: sampleWidth
        height: sampleHeight

        spacing: 16

        ThemeSample {
            theme: modelData

            onClicked: {
                root.themeChangeRequested(modelData.codeKey)
            }
        }

        RoundedRadioButton {
            width: parent.width
            checked: root.currentThemeCode === modelData.codeKey
            text: modelData.title

            navigation.name: text
            navigation.panel: root.navigationPanel
            navigation.row: root.navigationRow
            navigation.column: root.navigationColumnStart + model.index

            onToggled: {
                root.themeChangeRequested(modelData.codeKey)
            }
        }
    }
}
