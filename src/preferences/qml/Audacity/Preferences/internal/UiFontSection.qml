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

BaseSection {
    id: root

    title: qsTrc("preferences", "Appearance")

    property alias allFonts: selectFontControl.model

    property alias currentFontIndex: selectFontControl.currentIndex
    property alias bodyTextSize: bodyTextSizeControl.currentValue
    property string interfaceScale: "auto"
    property real systemInterfaceScale: 1.0

    signal fontChangeRequested(var newFontIndex)
    signal bodyTextSizeChangeRequested(var newBodyTextSize)
    signal interfaceScaleChangeRequested(string newScale)

    ComboBoxWithTitle {
        id: selectFontControl

        title: qsTrc("preferences", "Font face")
        columnWidth: root.columnWidth

        navigation.name: "FontFaceBox"
        navigation.panel: root.navigation
        navigation.row: 1

        onValueEdited: function (newIndex, newValue) {
            root.fontChangeRequested(newIndex)
        }
    }

    IncrementalPropertyControlWithTitle {
        id: bodyTextSizeControl

        title: qsTrc("preferences", "Body text size")
        columnWidth: root.columnWidth
        control.width: 112

        minValue: 10
        maxValue: 18

        //: Abbreviation of "pixels", used to specify a font size
        measureUnitsSymbol: qsTrc("global", "px")

        navigation.name: "BodyTextControl"
        navigation.panel: root.navigation
        navigation.row: 2

        onValueEdited: function (newValue) {
            root.bodyTextSizeChangeRequested(newValue)
        }
    }

    ComboBoxWithTitle {
        id: interfaceScaleControl

        //: The size of the whole interface: text, buttons, toolbars
        title: qsTrc("preferences", "Interface scale (after restart)")
        columnWidth: root.columnWidth

        model: [
            //: %1 is the scale that follows the desktop's font size, e.g. 125
            { text: qsTrc("preferences", "Automatic (%1 %)").arg(Math.round(root.systemInterfaceScale * 100)), value: "auto" },
            { text: "100 %", value: "1" },
            { text: "125 %", value: "1.25" },
            { text: "150 %", value: "1.5" },
            { text: "175 %", value: "1.75" },
            { text: "200 %", value: "2" }
        ]
        currentIndex: Math.max(0, control.indexOfValue(root.interfaceScale))

        navigation.name: "InterfaceScaleBox"
        navigation.panel: root.navigation
        navigation.row: 3

        onValueEdited: function (newIndex, newValue) {
            root.interfaceScaleChangeRequested(newValue)
        }
    }
}
