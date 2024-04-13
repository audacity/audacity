/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
 *
 * Copyright (C) 2024 Audacity BVBA and others
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

import Muse.Ui 1.0
import Muse.UiComponents 1.0

Item {
    id: root

    property alias listTitle: title.text
    property alias model: view.model

    property alias currentIndex: view.currentIndex

    property alias searchEnabled: searchField.visible
    property alias searchText: searchField.searchText
    property alias searching: searchField.hasText

    property alias navigationPanel: view.navigation

    signal titleClicked(var index)

    signal doubleClicked(var index)

    function clearSearch() {
        searchField.clear()
    }

    StyledTextLabel {
        id: title

        anchors.top: parent.top

        font: ui.theme.bodyBoldFont
    }

    SearchField {
        id: searchField

        anchors.top: title.bottom
        anchors.topMargin: 16

        navigation.name: "Search"
        navigation.panel: view.navigation
        navigation.row: 1

        width: parent.width
    }

    StyledListView {
        id: view

        anchors.top: searchEnabled ? searchField.bottom : title.bottom
        anchors.topMargin: 16
        anchors.bottom: parent.bottom

        width: parent.width
        spacing: 0

        currentIndex: 0

        accessible.name: title.text

        delegate: ListItemBlank {
            id: item

            isSelected: view.currentIndex === model.index

            navigation.name: modelData
            navigation.panel: view.navigation
            navigation.row: 2 + model.index
            navigation.accessible.name: titleLabel.text
            navigation.accessible.row: model.index

            StyledTextLabel {
                id: titleLabel

                anchors.fill: parent
                anchors.leftMargin: 12

                horizontalAlignment: Text.AlignLeft
                text: modelData
                font: ui.theme.bodyBoldFont
            }

            onClicked: {
                root.titleClicked(model.index)
            }

            onDoubleClicked: {
                root.doubleClicked(model.index)
            }
        }
    }

    StyledTextLabel {
        id: noResultsFoundHint

        anchors.fill: parent

        font: ui.theme.bodyBoldFont

        text: qsTrc("global", "No results found")

        visible: view.count < 1
    }
}
