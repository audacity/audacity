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

import Muse.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Scroll pages")

    property int orientation: Qt.Horizontal
    property alias limitScrollArea: limitScrollAreaBox.checked

    signal orientationChangeRequested(int orientation)
    signal limitScrollAreaChangeRequested(bool limit)

    RadioButtonGroup {
        id: radioButtonList

        width: parent.width
        height: implicitHeight

        spacing: 12
        orientation: ListView.Vertical

        model: [
            { title: qsTrc("appshell/preferences", "Horizontal"), value: Qt.Horizontal },
            { title: qsTrc("appshell/preferences", "Vertical"), value: Qt.Vertical }
        ]

        delegate: RoundedRadioButton {
            leftPadding: 0
            spacing: 6

            property string title: modelData["title"]

            checked: root.orientation === modelData["value"]

            navigation.name: "ScrollPagesOrientationButton"
            navigation.panel: root.navigation
            navigation.row: model.index
            navigation.accessible.name: title

            StyledTextLabel {
                text: title
                horizontalAlignment: Text.AlignLeft
            }

            onToggled: {
                root.orientationChangeRequested(modelData["value"])
            }
        }
    }

    CheckBox {
        id: limitScrollAreaBox
        width: parent.width

        text: qsTrc("appshell/preferences", "Limit scroll area to page borders")

        navigation.name: "LimitScrollAreaBox"
        navigation.panel: root.navigation
        navigation.row: radioButtonList.model.length

        onClicked: {
            root.limitScrollAreaChangeRequested(!checked)
        }
    }
}
