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
import MuseScore.Dock 1.0

DockToolBarView {
    id: root

    default property alias contentComponent: contentLoader.sourceComponent

    property int gripButtonPadding: 2
    property int contentTopPadding: 0
    property int contentBottomPadding: 0

    property int thickness: 36

    readonly property bool isVertical: root.orientation === Qt.Vertical

    minimumWidth: root.inited ? Math.min(root.contentWidth, root.maximumWidth) : prv.minimumLength
    minimumHeight: root.inited ? Math.min(root.contentHeight, root.maximumHeight) : prv.minimumLength

    contentNavigationPanel: Boolean(contentLoader.item) && Boolean(contentLoader.item.navigationPanel) ?
                                contentLoader.item.navigationPanel : null

    onFloatingChanged: {
        if (!root.floating) {
            //! NOTE: The dock widgets system determines the position of a toolbar
            //  when inserting the toolbar into the app window.
            //  It may be that the grip button can be moved to a different
            //  location from where a user wanted to place it.
            //  Because of this, the mouse area does not emit a signal
            //  that the user has moved the mouse outside the grip button.
            //  Therefore, the hover state of the grip button is not reset.
            //  The hack is to hide and show the grip button to reset the hover state.
            gripButton.visible = false
            gripButton.visible = true
        }
    }

    QtObject {
        id: prv

        readonly property int minimumLength: 10
        readonly property int maximumLength: 16777215

        readonly property int gripButtonWidth: gripButton.visible ? gripButton.width + 2 * root.gripButtonPadding : 0
        readonly property int gripButtonHeight: gripButton.visible ? gripButton.height + 2 * root.gripButtonPadding : 0
    }

    Item {
        id: content

        anchors.fill: parent
        anchors.topMargin: root.contentTopPadding
        anchors.bottomMargin: root.contentBottomPadding

        FlatButton {
            id: gripButton

            width: root.isVertical ? 30 : 24
            height: root.isVertical ? 24 : 30

            visible: root.floatable

            mouseArea.objectName: root.objectName + "_gripButton"
            mouseArea.cursorShape: Qt.SizeAllCursor

            transparent: true
            contentItem: StyledIconLabel {
                iconCode: IconCode.TOOLBAR_GRIP
                rotation: root.isVertical ? 90 : 0
            }

            Component.onCompleted: {
                root.setDraggableMouseArea(gripButton.mouseArea)
            }
        }

        Loader {
            id: contentLoader
        }
    }

    states: [
        State {
            name: "HORIZONTAL"
            when: !root.isVertical

            PropertyChanges {
                target: root

                contentWidth: prv.gripButtonWidth + contentLoader.implicitWidth
                contentHeight: Math.max(prv.gripButtonHeight, contentLoader.implicitHeight + root.contentBottomPadding + root.contentTopPadding)

                maximumWidth: root.floating ? root.contentWidth : prv.maximumLength
                maximumHeight: root.floating ? root.contentHeight : root.thickness
            }

            PropertyChanges {
                target: gripButton

                anchors.leftMargin: root.gripButtonPadding
            }

            AnchorChanges {
                target: gripButton

                anchors.left: parent.left
                anchors.verticalCenter: parent.verticalCenter
            }

            PropertyChanges {
                target: contentLoader

                anchors.leftMargin: gripButton.visible ? root.gripButtonPadding : 0
            }

            AnchorChanges {
                target: contentLoader

                anchors.left: gripButton.visible ? gripButton.right : parent.left
                anchors.verticalCenter: parent.verticalCenter
            }
        },

        State {
            name: "VERTICAL"
            when: root.isVertical

            PropertyChanges {
                target: root

                contentWidth: Math.max(prv.gripButtonWidth, contentLoader.implicitWidth + root.contentBottomPadding + root.contentTopPadding)
                contentHeight: prv.gripButtonHeight + contentLoader.implicitHeight

                maximumWidth: root.thickness
                maximumHeight: root.floating ? root.contentHeight : prv.maximumLength
            }

            PropertyChanges {
                target: gripButton

                anchors.topMargin: root.gripButtonPadding
            }

            AnchorChanges {
                target: gripButton

                anchors.top: parent.top
                anchors.horizontalCenter: parent.horizontalCenter
            }

            PropertyChanges {
                target: contentLoader

                anchors.topMargin: gripButton.visible ? root.gripButtonPadding : 0
            }

            AnchorChanges {
                target: contentLoader

                anchors.top: gripButton.visible ? gripButton.bottom : parent.top
                anchors.horizontalCenter: parent.horizontalCenter
            }
        }
    ]
}
