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

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Zoom")

    navigation.direction: NavigationPanel.Both

    property var defaultZoom: null
    property alias zoomTypes: defaultZoomTypesBox.model
    property alias mouseZoomPrecision: mouseZoomPrecisionControl.currentValue

    signal defaultZoomTypeChangeRequested(int zoomType)
    signal defaultZoomLevelChangeRequested(int zoomLevel)
    signal mouseZoomPrecisionChangeRequested(int zoomPrecision)

    Row {
        spacing: root.columnSpacing

        ComboBoxWithTitle {
            id: defaultZoomTypesBox

            title: qsTrc("appshell/preferences", "Default zoom:")
            columnWidth: root.columnWidth

            control.textRole: "title"
            control.valueRole: "value"

            currentIndex: control.indexOfValue(root.defaultZoom.type)

            navigation.name: "DefaultZoomBox"
            navigation.panel: root.navigation
            navigation.row: 0
            navigation.column: 0

            onValueEdited: function(newIndex, newValue) {
                root.defaultZoomTypeChangeRequested(newValue)
            }
        }

        IncrementalPropertyControl {
            id: defaultZoomControl
            width: 64

            maxValue: 1600
            minValue: 10
            step: 10
            decimals: 0

            measureUnitsSymbol: "%"

            currentValue: root.defaultZoom.level
            enabled: root.defaultZoom.isPercentage

            navigation.name: "DefaultZoomControl"
            navigation.panel: root.navigation
            navigation.row: 0
            navigation.column: 1

            onValueEdited: function(newValue) {
                root.defaultZoomLevelChangeRequested(newValue)
            }
        }
    }

    IncrementalPropertyControlWithTitle {
        id: mouseZoomPrecisionControl

        title: qsTrc("appshell/preferences", "Mouse zoom precision:")

        columnWidth: root.columnWidth
        control.width: 60

        minValue: 1
        maxValue: 16

        navigation.name: "MouseZoomPrecisionControl"
        navigation.panel: root.navigation
        navigation.row: 1
        navigation.column: 0

        onValueEdited: function(newValue) {
            root.mouseZoomPrecisionChangeRequested(newValue)
        }
    }
}
