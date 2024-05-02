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
import QtQuick.Layouts 1.12

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import "qrc:/kddockwidgets/private/quick/qml/" as KDDW

Item {
    id: root

    required property QtObject titleBarCpp

    property alias contextMenuModel: contextMenuButton.menuModel
    property alias heightWhenVisible: titleBar.heightWhenVisible
    property bool isHorizontalPanel: false

    property alias navigation: contextMenuButton.navigation

    signal handleContextMenuItemRequested(string itemId)

    width: parent.width
    height: visible ? heightWhenVisible : 0

    visible: Boolean(titleBarCpp)

    KDDW.TitleBarBase {
        id: titleBar

        anchors.fill: parent

        heightWhenVisible: titleBarContent.implicitHeight
        color: ui.theme.backgroundPrimaryColor

        visible: parent.visible

        MouseArea {
            id: mouseArea
            anchors.fill: parent
            acceptedButtons: Qt.NoButton
            cursorShape: Qt.SizeAllCursor
        }

        Column {
            id: titleBarContent

            anchors.fill: parent
            anchors.leftMargin: 12
            anchors.rightMargin: 12

            spacing: 0

            RowLayout {
                width: parent.width
                height: 34

                StyledTextLabel {
                    id: titleLabel
                    Layout.fillWidth: true

                    text: titleBar.title
                    font: ui.theme.bodyBoldFont
                    horizontalAlignment: Qt.AlignLeft
                }

                MenuButton {
                    id: contextMenuButton

                    width: 20
                    height: width

                    onHandleMenuItem: function(itemId) {
                        root.handleContextMenuItemRequested(itemId)
                    }
                }
            }

            SeparatorLine {
                id: bottomSeparator
                orientation: Qt.Horizontal
                anchors.margins: -12
                visible: root.isHorizontalPanel
            }
        }
    }
}
