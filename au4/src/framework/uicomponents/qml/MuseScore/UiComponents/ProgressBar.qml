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

import MuseScore.Ui 1.0
import MuseScore.GraphicalEffects 1.0

FocusScope {
    id: root

    property real from: 0.0
    property real to: 1.0
    property real value: 0.0

    property alias progressStatus: progressStatusLabel.text

    property alias navigation: navCtrl

    function ensureActiveFocus() {
        if (!root.activeFocus) {
            root.forceActiveFocus()
        }
    }

    NavigationControl {
        id: navCtrl

        name: root.objectName != "" ? root.objectName : "ProgressBar"
        enabled: root.enabled && root.visible
        panel: navCtrl.panel
        order: 1000

        accessible.role: MUAccessible.Range
        accessible.name: root.progressStatus
        accessible.ignored: true
        accessible.visualItem: root

        accessible.value: {
            var current = Math.trunc((root.value * 100) / root.to) // to percent
            if (current % 10 !== 0) {
                return accessible.value
            }

            return current
        }
        accessible.minimumValue: 0
        accessible.maximumValue: 100
        accessible.stepSize: 1

        onActiveChanged: function(active) {
            if (active) {
                accessible.ignored = false
            }
        }
    }

    Rectangle {
        id: backgroundRect

        anchors.fill: parent

        color: ui.theme.backgroundSecondaryColor

        NavigationFocusBorder { navigationCtrl: navCtrl }

        radius: 3
        border.color: ui.theme.strokeColor
        border.width: Math.max(ui.theme.borderWidth, 1)

        layer.enabled: true
        layer.effect: EffectOpacityMask {
            maskSource: Item {
                width: backgroundRect.width
                height: backgroundRect.height

                Rectangle {
                    anchors.fill: parent
                    radius: backgroundRect.radius
                }
            }
        }

        Rectangle {
            id: progressRect

            anchors.top: parent.top
            anchors.left: parent.left
            anchors.bottom: parent.bottom
            anchors.margins: backgroundRect.border.width

            width: Math.min(parent.width * (root.value / root.to), parent.width - backgroundRect.border.width)

            color: ui.theme.accentColor
        }

        StyledTextLabel {
            id: progressStatusLabel

            anchors.horizontalCenter: parent.horizontalCenter
            anchors.verticalCenter: parent.verticalCenter

            horizontalAlignment: Text.AlignHCenter

            z: progressRect.z + 1
        }
    }
}
