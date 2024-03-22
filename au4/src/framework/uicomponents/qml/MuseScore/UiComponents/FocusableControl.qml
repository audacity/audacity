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

FocusScope {
    id: root

    default property alias content: contentItemContainer.data
    property alias contentItem: contentItemContainer
    property alias background: focusRectItem
    property alias focusBorder: focusBorderItem

    property alias mouseArea: mouseAreaItem

    property alias navigation: navCtrl

    signal navigationActivated()
    signal navigationTriggered()

    function ensureActiveFocus() {
        if (!root.activeFocus) {
            root.forceActiveFocus()
        }
    }

    //! NOTE ListView can destroy delegates, but not delete objects,
    // they remain in memory (this is done for optimization, for reusing delegate objects).
    // In this case, navigation controls also remain in memory and in the navigation tree.
    // But they should at least be turned off.
    property bool completed: false
    Component.onCompleted: root.completed = true
    Component.onDestruction: root.completed = false

    NavigationControl {
        id: navCtrl
        name: root.objectName
        enabled: root.enabled && root.visible && root.completed

        onActiveChanged: {
            if (navCtrl.active) {
                root.ensureActiveFocus()
                root.navigationActivated()
            }
        }

        onTriggered: {
            root.navigationTriggered()
        }
    }

    Rectangle {
        id: focusRectItem
        anchors.fill: parent

        color: ui.theme.backgroundPrimaryColor
        opacity: 1

        NavigationFocusBorder {
            id: focusBorderItem
            navigationCtrl: navCtrl
        }

        border.color: ui.theme.strokeColor
        border.width: ui.theme.borderWidth
    }

    MouseArea {
        id: mouseAreaItem
        anchors.fill: parent

        onClicked: {
            root.ensureActiveFocus()
        }
    }

    Item {
        id: contentItemContainer
        objectName: "FocusableControlContent"
        anchors.fill: focusRectItem
    }
}
