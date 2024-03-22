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
import QtQuick 2.9
import QtQuick.Layouts 1.3

import MuseScore.Ui 1.0

Rectangle {
    id: root

    property int orientation: privateProperties.parentIsHorizontal ? Qt.Vertical : Qt.Horizontal

    color: ui.theme.strokeColor

    QtObject {
        id: privateProperties

        readonly property bool parentIsHorizontal: root.parent instanceof Row || root.parent instanceof RowLayout
        readonly property bool parentIsLayout: root.parent instanceof ColumnLayout || root.parent instanceof RowLayout || root.parent instanceof GridLayout
    }

    states: [
        State {
            name: "HORIZONTAL"
            when: orientation == Qt.Horizontal

            PropertyChanges {
                target: root
                height: 1
                Layout.fillWidth: true
            }

            StateChangeScript {
                script: {
                    if (privateProperties.parentIsLayout) {
                        root.Layout.fillWidth = true
                    } else {
                        root.anchors.left = root.parent.left
                        root.anchors.right = root.parent.right
                    }
                }
            }
        },

        State {
            name: "VERTICAL"
            when: orientation == Qt.Vertical

            PropertyChanges {
                target: root
                width: 1
                Layout.fillHeight: true
            }

            StateChangeScript {
                script: {
                    if (privateProperties.parentIsLayout) {
                        root.Layout.fillHeight = true
                    } else {
                        root.anchors.top = root.parent.top
                        root.anchors.bottom = root.parent.bottom
                    }
                }
            }
        }
    ]
}
