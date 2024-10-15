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

StyledDialogView {
    id: root

    property bool rememberChoice

    contentHeight: 355
    contentWidth: 496

    margins: 24

    function done(data = {}) {
        let value = Object.assign(data)

        root.ret = {
            errcode: 0,
            value: value
        }

        root.hide()
    }

    onNavigationActivateRequested: {
        var btn = buttonBox.firstFocusBtn
        if (btn) {
            btn.navigation.requestActive()
        }
    }

    onAccessibilityActivateRequested: {
        accessibleInfo.readInfo()
    }

    NavigationPanel {
        id: buttonsPanel

        name: "ButtonsPanel"
        order: 1
        section: root.navigationSection
        direction: NavigationPanel.Horizontal
        accessible.role: MUAccessible.Dialog

        onNavigationEvent: function(event) {
            if (event.type === NavigationEvent.AboutActive) {
                var btn = buttonBox.firstFocusBtn
                if (Boolean(btn) && btn.enabled) {
                    event.setData("controlIndex", [ btn.navigation.row, btn.navigation.column ])
                }
            } else {
                buttonBox.restoreAccessibility()
                accessibleInfo.resetFocus()
            }
        }
    }

    AccessibleItem {
        id: accessibleInfo

        accessibleParent: buttonsPanel.accessible
        role: MUAccessible.Button
        name: titleInfo.text + "; " + subtitleInfo.text

        function readInfo() {
            accessibleInfo.ignored = false
            accessibleInfo.focused = true
        }

        function resetFocus() {
            accessibleInfo.ignored = true
            accessibleInfo.focused = false
        }
    }

    ColumnLayout {
        id: content

        anchors.fill: parent
        spacing: 20

        ColumnLayout {
            id: header

            width: parent.width
            spacing: 16

            StyledTextLabel {
                id: titleInfo

                Layout.fillWidth: true

                text: qsTrc("project/cloud", "Would you also like to share your music on Audio.com?")
                font: ui.theme.largeBodyBoldFont
            }

            StyledTextLabel {
                id: subtitleInfo

                Layout.fillWidth: true

                text: qsTrc("project/cloud", "Share your project’s audio with millions of listeners on this free streaming platform")
                font: ui.theme.bodyFont
            }
        }

        Image {
            Layout.fillWidth: true
            Layout.preferredHeight: sourceSize.height * (width / sourceSize.width)
            fillMode: Image.PreserveAspectFit

            source: "qrc:/resources/AudioCom_Waveform.png"
        }

        StyledTextLabel {
            id: preferenceInfo

            Layout.fillWidth: true
            horizontalAlignment: Text.AlignLeft

            text: qsTrc("project/cloud", "You can change this setting in Preferences at any time.")
            font: ui.theme.bodyFont
        }

        RowLayout {
            id: options

            CheckBox {
                id: checkbox

                Layout.fillWidth: true

                navigation.panel: NavigationPanel {
                    name: "RememberChoiceCheckBox"
                    section: root.navigationSection
                    order: 2
                }
                navigation.row: 1
                navigation.accessible.name: text + "; " + preferenceInfo.text

                text: qsTrc("project/cloud", "Remember my choice")

                checked: root.rememberChoice

                onClicked: {
                    checked = !checked
                }
            }

            ButtonBox {
                id: buttonBox

                buttons: [ ButtonBoxModel.No, ButtonBoxModel.Yes ]

                navigationPanel: buttonsPanel
                isAccessibilityDisabledWhenInit: true

                onStandardButtonClicked: function(buttonId) {
                    root.done({ share: buttonId === ButtonBoxModel.Yes, remember: checkbox.checked })
                }
            }
        }
    }
}
