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

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0

Item {
    id: root

    property var model

    property alias navigationPanel: view.navigationPanel

    function focusOnFirst() {
        var firstItem = view.itemAtIndex(0)
        if (Boolean(firstItem)) {
            firstItem.navigation.requestActive()
        }
    }

    QtObject {
        id: prv

        readonly property int sideMargin: 36
        property string currentItemNavigationName: ""

        function resolveCurrentNavigationName() {
            if (currentItemNavigationName == "") {
                return ""
            }

            for (var i = 0; i < view.count; ++i) {
                var item = view.itemAtIndex(i)
                if (currentItemNavigationName === item.navigation.name) {
                    return currentItemNavigationName
                }
            }

            return ""
        }
    }

    Column {
        id: header

        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.leftMargin: prv.sideMargin

        spacing: 16

        StyledTextLabel {
            width: parent.width

            text: qsTrc("ui", "Path")

            horizontalAlignment: Qt.AlignLeft
            font.capitalization: Font.AllUppercase
        }

        SeparatorLine { anchors.margins: -prv.sideMargin }
    }

    StyledListView {
        id: view

        anchors.top: header.bottom
        anchors.bottom: parent.bottom
        width: parent.width

        spacing: 0

        model: root.model

        interactive: height < contentHeight

        property NavigationPanel navigationPanel: NavigationPanel {
            name: "DirectoriesView"
            enabled: root.enabled && root.visible
            direction: NavigationPanel.Both
            accessible.name: qsTrc("ui", "Directories list")
            onActiveChanged: function(active) {
                if (active) {
                    root.forceActiveFocus()
                }
            }

            onNavigationEvent: function(event) {
                if (event.type === NavigationEvent.AboutActive) {
                    var controlName = prv.resolveCurrentNavigationName()
                    if (controlName !== "") {
                        event.setData("controlName", controlName)
                    }
                }
            }
        }

        ScrollBar.vertical: StyledScrollBar {}

        Connections {
            target: root.model

            function onDirectoryAdded(index) {
                view.positionViewAtIndex(index, ListView.Contain)
                view.currentIndex = index
            }
        }

        delegate: ListItemBlank {
            normalColor: ui.theme.backgroundPrimaryColor

            isSelected: selectedRole

            navigation.name: model.index
            navigation.panel: view.navigationPanel
            navigation.row: model.index
            navigation.accessible.name: titleRole
            navigation.onActiveChanged: {
                if (navigation.active) {
                    prv.currentItemNavigationName = navigation.name
                    view.positionViewAtIndex(index, ListView.Contain)
                }
            }

            onClicked: {
                root.model.selectRow(index)
            }

            StyledTextLabel {
                anchors.fill: parent
                anchors.leftMargin: prv.sideMargin
                anchors.rightMargin: prv.sideMargin

                text: titleRole
                horizontalAlignment: Text.AlignLeft
                elide: Text.ElideMiddle
            }
        }
    }
}
