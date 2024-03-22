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

FocusScope {
    id: root

    property alias title: titleLabel.text
    property alias titleFont: titleLabel.font

    property alias menuItemComponent: menuLoader.sourceComponent

    property bool isExpanded: true

    property alias navigation: navCtrl

    anchors.left: parent.left
    anchors.leftMargin: -expandButtonIcon.width / 3
    anchors.right: parent.right

    implicitHeight: expandSectionRow.height

    NavigationControl {
        id: navCtrl
        name: root.title
        enabled: root.enabled && root.visible
        accessible.role: MUAccessible.ListItem
        accessible.name: root.title

        onTriggered: {
            root.isExpanded = !root.isExpanded
        }
    }

    NavigationFocusBorder{
        navigationCtrl: navCtrl
        anchors.margins: 0
    }

    Row {
        id: expandSectionRow

        spacing: 4

        height: titleLabel.implicitHeight

        Rectangle {
            id: expandButton

            anchors.verticalCenter: parent.verticalCenter

            height: expandButtonIcon.height * 1.2
            width: expandButtonIcon.width * 1.2

            color: "transparent"

            StyledIconLabel {
                id: expandButtonIcon

                anchors.verticalCenter: parent.verticalCenter

                rotation: root.isExpanded ? 0 : -90

                iconCode: IconCode.SMALL_ARROW_DOWN

                Behavior on rotation {
                    NumberAnimation {
                        easing.type: Easing.OutQuad
                        duration: 50
                    }
                }
            }
        }

        StyledTextLabel {
            id: titleLabel

            anchors.verticalCenter: expandButton.verticalCenter

            font: ui.theme.bodyBoldFont
        }
    }

    MouseArea {
        id: mouseArea

        anchors.fill: expandSectionRow

        hoverEnabled: true

        onClicked: {
            navigation.requestActiveByInteraction()

            root.isExpanded = !root.isExpanded
        }
    }

    Loader {
        id: menuLoader

        property bool isMenuButtonVisible: root.isExpanded || mouseArea.containsMouse

        anchors {
            right: root.right
            rightMargin: 48
            top: expandSectionRow.top
        }

        height: childrenRect.height
        width: childrenRect.width
    }

    states: [
        State {
            name: "DISABLED"
            when: !root.enabled

            PropertyChanges { target: root; isExpanded: false; opacity: ui.theme.itemOpacityDisabled }
        }
    ]
}
