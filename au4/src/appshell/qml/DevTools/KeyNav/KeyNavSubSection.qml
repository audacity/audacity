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
import MuseScore.UiComponents 1.0

Rectangle {

    id: root

    property alias keynavSection: keynavsec.section
    property alias subsectionName: keynavsec.name
    property alias subsectionOrder: keynavsec.order

    height: 40
    width: btns.childrenRect.width

    NavigationFocusBorder { navigationCtrl: keynavsec }

    signal clicked(string info)

    function doClicked(control) {
        var info = "sub: " + root.subsectionName + ", control: " + control
        console.log(info)
        root.clicked(info)
    }

    NavigationPanel {
        id: keynavsec

        onActiveChanged: {
            if (keynavsec.active) {
                root.forceActiveFocus()
            }
        }
    }

    Row {
        id: btns
        anchors.fill: parent
        spacing: 8

        FlatButton {
            id: btn1
            navigation.panel: keynavsec
            navigation.order: 1
            anchors.verticalCenter: parent.verticalCenter
            height: 24
            width: 24
            text: "C1"
            onClicked: root.doClicked(text)
        }

        FlatButton {
            id: btn2
            navigation.panel: keynavsec
            navigation.order: 2
            anchors.verticalCenter: parent.verticalCenter
            height: 24
            width: 24
            text: "C2"
            onClicked: root.doClicked(text)
        }

        FlatButton {
            id: btn3
            navigation.panel: keynavsec
            navigation.order: 3
            anchors.verticalCenter: parent.verticalCenter
            height: 24
            width: 24
            text: "C3"
            onClicked: root.doClicked(text)
        }
    }
}
