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
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

ColumnLayout {
    id: root

    property alias title: titleLabel.text
    property alias description: descriptionLabel.text
    property alias buttonText: button.text

    property alias imageSource: image.source

    property alias navigation: button.navigation

    signal buttonClicked

    readonly property int radius: 6

    spacing: 0

    RoundedRectangle {
        Layout.fillWidth: true
        implicitHeight: 208

        color: ui.theme.isDark ? "#44495A" : "#D7DEE5"

        topLeftRadius: root.radius
        topRightRadius: root.radius

        Image {
            id: image
            anchors.fill: parent

            fillMode: Image.PreserveAspectFit
        }
    }

    RoundedRectangle {
        Layout.fillWidth: true
        Layout.fillHeight: true

        color: ui.theme.backgroundSecondaryColor

        bottomLeftRadius: root.radius
        bottomRightRadius: root.radius

        ColumnLayout {
            anchors.fill: parent
            anchors.margins: 24
            spacing: 24

            StyledTextLabel {
                id: titleLabel
                Layout.fillWidth: true
                font: ui.theme.headerBoldFont
                horizontalAlignment: Text.AlignLeft
            }

            StyledTextLabel {
                id: descriptionLabel
                Layout.fillWidth: true
                Layout.fillHeight: true
                wrapMode: Text.WordWrap
                maximumLineCount: 0
                font: ui.theme.largeBodyFont
                horizontalAlignment: Text.AlignLeft
                verticalAlignment: Text.AlignTop
            }

            FlatButton {
                id: button
                accentButton: true

                onClicked: {
                    root.buttonClicked()
                }
            }
        }
    }
}
