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
import QtQuick.Layouts 1.15

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0

StyledDialogView {
    id: root

    property alias type: mainPanel.type

    property alias title: mainPanel.title
    property alias text: mainPanel.text
    property alias textFormat: mainPanel.textFormat
    property string detailedText: ""

    property alias withIcon: mainPanel.withIcon
    property alias iconCode: mainPanel.iconCode

    property alias withDontShowAgainCheckBox: mainPanel.withDontShowAgainCheckBox

    property var buttons
    property var customButtons
    property alias defaultButtonId: mainPanel.defaultButtonId

    QtObject {
        id: toggleDetailsButton

        property int buttonId: 999
        property string text: detailsLoader.active ? qsTrc("global", "Hide details") : qsTrc("global", "Show details")
        property int role: ButtonBoxModel.CustomRole
        property bool isAccent: false
        property bool isLeftSide: true
    }

    contentWidth: mainPanel.implicitWidth
    contentHeight: content.implicitHeight

    margins: 16

    onDetailedTextChanged: {
        if (root.detailedText.length <= 0) {
            return
        }

        var tmp = []
        tmp.push(toggleDetailsButton)

        for (var i = 0; i < root.customButtons.length; ++i) {
            tmp.push(root.customButtons[i])
        }

        root.customButtons = tmp
    }

    onNavigationActivateRequested: {
        mainPanel.focusOnFirst()
    }

    onAccessibilityActivateRequested: {
        mainPanel.readInfo()
    }

    Column {
        id: content

        spacing: 16

        StandardDialogPanel {
            id: mainPanel

            navigation.section: root.navigationSection
            navigation.order: 1

            buttons: root.buttons
            customButtons: root.customButtons

            onClicked: function(buttonId, showAgain) {
                if (buttonId === toggleDetailsButton.buttonId) {
                    detailsLoader.active = !detailsLoader.active
                    return
                }

                root.ret = { "errcode": 0, "value": { "buttonId": buttonId, "showAgain": showAgain }}
                root.hide()
            }
        }

        Loader {
            id: detailsLoader

            width: parent.width
            height: visible ? implicitHeight : 0

            active: false
            visible: active

            sourceComponent: ErrorDetailsView {
                detailedText: root.detailedText

                navigationSection: root.navigationSection
                navigationOrder: 2
            }
        }
    }
}
