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
import QtQuick.Controls 2.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Zoom")

    navigation.direction: NavigationPanel.Both

    property var defaultZoom: null
    property alias zoomTypes: defaultZoomTypesBox.model
    property alias mouseZoomPrecision: precisionSlider.value

    signal defaultZoomTypeChangeRequested(int zoomType)
    signal defaultZoomLevelChangeRequested(int zoomLevel)
    signal mouseZoomPrecisionChangeRequested(int zoomPrecision)

    Row {
        visible: root.defaultZoom !== null

        spacing: root.columnSpacing

        ComboBoxWithTitle {
            id: defaultZoomTypesBox

            title: qsTrc("appshell/preferences", "Default zoom:")
            columnWidth: root.columnWidth

            control.textRole: "title"
            control.valueRole: "value"

            currentIndex: root.defaultZoom ? control.indexOfValue(root.defaultZoom.type) : -1

            navigation.name: "DefaultZoomBox"
            navigation.panel: root.navigation
            navigation.row: 0
            navigation.column: 0

            onValueEdited: function (newIndex, newValue) {
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

            currentValue: root.defaultZoom ? root.defaultZoom.level : 100
            enabled: root.defaultZoom ? root.defaultZoom.isPercentage : false

            navigation.name: "DefaultZoomControl"
            navigation.panel: root.navigation
            navigation.row: 0
            navigation.column: 1

            onValueEdited: function (newValue) {
                root.defaultZoomLevelChangeRequested(newValue)
            }
        }
    }

    Column {
        id: mouseZoomPrecisionControl

        spacing: 6

        StyledTextLabel {
            text: qsTrc("appshell/preferences", "Mouse zoom precision")
            width: root.columnWidth
            horizontalAlignment: Qt.AlignLeft
            wrapMode: Text.WordWrap
            maximumLineCount: 2
        }

        Row {
            spacing: 8

            StyledTextLabel {
                text: qsTrc("appshell/preferences", "Fast")
                width: 40
                horizontalAlignment: Qt.AlignLeft
            }

            StyledSlider {
                id: precisionSlider

                width: 220
                from: 1
                to: 16
                stepSize: 1
                snapMode: Slider.SnapAlways

                navigation.name: "MouseZoomPrecisionControl"
                navigation.panel: root.navigation
                navigation.row: 1
                navigation.column: 0
                navigation.accessible.name: qsTrc("appshell/preferences", "Mouse zoom precision")

                onMoved: {
                    root.mouseZoomPrecisionChangeRequested(Math.round(value))
                }
            }

            StyledTextLabel {
                text: qsTrc("appshell/preferences", "Slow")
                width: 40
                horizontalAlignment: Qt.AlignRight
            }
        }
    }
}
