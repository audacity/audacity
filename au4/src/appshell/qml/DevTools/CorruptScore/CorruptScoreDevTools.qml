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
import MuseScore.Diagnostics 1.0

Rectangle {
    color: ui.theme.backgroundSecondaryColor

    CorruptScoreDevToolsModel {
        id: model
    }

    Column {
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.margins: 12

        spacing: 12

        Rectangle {
            readonly property real padding: 12

            width: hintLabel.implicitWidth + 2 * padding
            height: hintLabel.implicitHeight + 2 * padding

            color: Utils.colorWithAlpha(border.color, 0.25)
            border.color: "red"
            border.width: 1
            radius: 6

            Row {
                id: hintLabel
                anchors.fill: parent
                anchors.margins: parent.padding
                spacing: 4

                StyledIconLabel {
                    iconCode: IconCode.WARNING
                }

                StyledTextLabel {
                    text: "DANGER! This button does exactly what it says!"
                }
            }
        }

        FlatButton {
            text: "Corrupt the open score"
            onClicked: model.corruptOpenScore()
        }
    }
}
