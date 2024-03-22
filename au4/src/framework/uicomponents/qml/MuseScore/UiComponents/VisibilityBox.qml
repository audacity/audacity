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
import QtQuick.Layouts 1.15

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0

FocusScope {
    id: root

    property alias text: label.text
    property string accessibleText: text
    property bool isVisible: true

    property alias navigation: eyeButton.navigation

    signal visibleToggled()

    implicitHeight: contentRow.implicitHeight
    implicitWidth: contentRow.implicitWidth

    opacity: root.enabled ? 1.0 : ui.theme.itemOpacityDisabled

    RowLayout {
        id: contentRow
        spacing: 2

        FlatButton {
            id: eyeButton

            Layout.alignment: Qt.AlignVCenter
            Layout.preferredWidth: width

            // Make mouse area fill the whole area to get desired hover effect
            readonly property rect mouseAreaRect: contentRow.mapToItem(this, 0, 0, contentRow.width, contentRow.height)

            mouseArea.anchors.fill: null
            mouseArea.x: mouseAreaRect.x
            mouseArea.y: mouseAreaRect.y
            mouseArea.width: mouseAreaRect.width
            mouseArea.height: mouseAreaRect.height

            icon: root.isVisible ? IconCode.EYE_OPEN : IconCode.EYE_CLOSED
            transparent: true

            accessible.name: (root.accessibleText ? root.accessibleText + ", " : "")
                             + (root.isVisible ? qsTrc("ui", "Visible") : qsTrc("ui", "Hidden"))

            onClicked: {
                root.visibleToggled()
            }
        }

        StyledTextLabel {
            id: label
            visible: !isEmpty

            readonly property real availableWidth: root.width - contentRow.spacing - eyeButton.width

            Layout.preferredWidth: availableWidth > 0 ? Math.min(availableWidth, label.implicitWidth) : label.implicitWidth
            Layout.alignment: Qt.AlignLeft | Qt.AlignVCenter

            horizontalAlignment: Text.AlignLeft
            wrapMode: Text.WordWrap
            maximumLineCount: 2
        }
    }
}
