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
import Audacity.Project 1.0

import "internal/SaveToCloud"

StyledDialogView {
    id: root

    contentHeight: 600
    contentWidth: 900

    objectName: "AskSaveLocationTypeDialog"

    property bool askAgain: true

    function done(saveLocationType) {
        root.ret = {
            errcode: 0,
            value: {
                askAgain: root.askAgain,
                saveLocationType: saveLocationType
            }
        }

        root.hide()
    }

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 24
        spacing: 24

        StyledTextLabel {
            Layout.fillWidth: true
            text: qsTrc("project/save", "How would you like to save?")
            font: ui.theme.headerBoldFont
        }

        RowLayout {
            id: optionsRowLayout
            spacing: 24

            NavigationPanel {
                id: optionsNavPanel
                name: "SaveLocationOptionsButtons"
                enabled: optionsRowLayout.enabled && optionsRowLayout.visible
                direction: NavigationPanel.Horizontal
                section: root.navigationSection
                order: 1
            }

            SaveLocationOption {
                title: qsTrc("project/save", "To the Cloud (free)")
                description: qsTrc("project/save", "Files are saved privately on your own personal account. \
You can share drafts with others and publish your finished projects publicly too.")
                buttonText: qsTrc("project/save", "Save to the cloud")

                imageSource: "qrc:/SaveToCloud/images/Cloud.png"

                navigation.panel: optionsNavPanel
                navigation.column: 1
                navigation.accessible.name: qsTrc("project/save", "Save to the cloud (free)")
                navigation.accessible.description: description

                onButtonClicked: {
                    root.done(SaveLocationType.Cloud)
                }
            }

            SaveLocationOption {
                title: qsTrc("project/save", "On your computer")
                description: qsTrc("project/save", "If you prefer to save your files on your computer, you can do that here.")
                buttonText: qsTrc("project/save", "Save to computer")

                imageSource: "qrc:/SaveToCloud/images/Laptop.png"

                navigation.panel: optionsNavPanel
                navigation.column: 2
                navigation.accessible.name: qsTrc("project/save", "Save on your computer")
                navigation.accessible.description: description

                onButtonClicked: {
                    root.done(SaveLocationType.Local)
                }
            }
        }

        SeparatorLine { Layout.margins: -24 }

        NavigationPanel {
            id: dontAskAgainPanel
            name: "DontAskAgain"
            enabled: dontAskAgainCheckbox.enabled && dontAskAgainCheckbox.visible
            section: root.navigationSection
            order: 2
            accessible.name: dontAskAgainCheckbox.text
        }

        CheckBox {
            id: dontAskAgainCheckbox

            width: parent.width
            text: qsTrc("global", "Don’t show again")
            checked: !root.askAgain

            navigation.panel: dontAskAgainPanel
            navigation.order: 1

            onClicked: {
                root.askAgain = !root.askAgain
            }
        }
    }
}
