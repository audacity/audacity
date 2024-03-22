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

import "internal"

Item {
    id: root

    property var model: null
    property int orientation: Qt.Horizontal
    readonly property bool isHorizontal: orientation === Qt.Horizontal

    property Component sectionDelegate: Item {}
    property Component itemDelegate: Item {}

    property alias contentWidth: loader.width
    property alias contentHeight: loader.height

    property int cellWidth: 0
    property int cellHeight: 0

    property int sectionWidth: 0
    property int sectionHeight: 0
    property string sectionRole: "sectionRole"

    readonly property int noLimit: -1
    property int rows: noLimit
    property int rowSpacing: 2
    property int columns: noLimit
    property int columnSpacing: 2

    QtObject {
        id: privateProperties

        property int spacingBeforeSection: isHorizontal ? columnSpacing : rowSpacing
        property int spacingAfterSection: spacingBeforeSection

        function modelSections() {
            var _sections = []

            for (var i = 0; i < root.model.length; i++) {
                var element = root.model.get(i)

                var section = element[sectionRole]
                if (!_sections.includes(section)) {
                    _sections.push(section)
                }
            }

            return _sections
        }
    }

    Loader {
        id: loader

        sourceComponent: isHorizontal ? horizontalView : verticalView
    }

    Component {
        id: horizontalView

        Row {
            spacing: privateProperties.spacingBeforeSection

            height: childrenRect.height

            Repeater {
                model: Boolean(root.model) ? privateProperties.modelSections() : []

                Row {
                    spacing: privateProperties.spacingAfterSection

                    height: root.sectionHeight

                    GridViewSection {
                        width: root.sectionWidth
                        height: root.sectionHeight

                        sectionDelegate: root.sectionDelegate
                    }

                    GridViewDelegate {
                        anchors.verticalCenter: parent.verticalCenter

                        model: Boolean(root.model) ? root.model : null

                        itemDelegate: root.itemDelegate
                        sectionRole: root.sectionRole

                        cellWidth: root.cellWidth
                        cellHeight: root.cellHeight

                        rows: root.rows
                        rowSpacing: root.rowSpacing
                        columns: root.columns
                        columnSpacing: root.columnSpacing
                    }
                }
            }
        }
    }

    Component {
        id: verticalView

        Column {
            spacing: privateProperties.spacingBeforeSection

            width: childrenRect.width

            Repeater {
                model: Boolean(root.model) ? privateProperties.modelSections() : []

                Column {
                    spacing: privateProperties.spacingAfterSection

                    width: root.sectionWidth

                    GridViewSection {
                        width: root.sectionWidth
                        height: root.sectionHeight

                        sectionDelegate: root.sectionDelegate
                    }

                    GridViewDelegate {
                        anchors.horizontalCenter: parent.horizontalCenter

                        model: Boolean(root.model) ? root.model : null

                        itemDelegate: root.itemDelegate
                        sectionRole: root.sectionRole

                        cellWidth: root.cellWidth
                        cellHeight: root.cellHeight

                        rows: root.rows
                        rowSpacing: root.rowSpacing
                        columns: root.columns
                        columnSpacing: root.columnSpacing
                    }
                }
            }
        }
    }
}
