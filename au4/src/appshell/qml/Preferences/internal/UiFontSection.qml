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

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Appearance")

    property alias allFonts: selectFontControl.model

    property alias currentFontIndex: selectFontControl.currentIndex
    property alias bodyTextSize: bodyTextSizeControl.currentValue

    signal fontChangeRequested(var newFontIndex)
    signal bodyTextSizeChangeRequested(var newBodyTextSize)

    ComboBoxWithTitle {
        id: selectFontControl

        title: qsTrc("appshell/preferences", "Font face:")
        columnWidth: root.columnWidth

        navigation.name: "FontFaceBox"
        navigation.panel: root.navigation
        navigation.row: 1

        onValueEdited: function(newIndex, newValue) {
            root.fontChangeRequested(newIndex)
        }
    }

    IncrementalPropertyControlWithTitle {
        id: bodyTextSizeControl

        title: qsTrc("appshell/preferences", "Body text size:")
        columnWidth: root.columnWidth
        control.width: 112

        minValue: 10
        maxValue: 18

        //: Abbreviation of "points", used to specify a font size
        measureUnitsSymbol: qsTrc("global", "pt")

        navigation.name: "BodyTextControl"
        navigation.panel: root.navigation
        navigation.row: 2

        onValueEdited: function(newValue) {
            root.bodyTextSizeChangeRequested(newValue)
        }
    }
}
