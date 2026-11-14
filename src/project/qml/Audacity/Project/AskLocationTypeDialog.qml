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
import Muse.UiComponents
import Audacity.Project 1.0

import "internal/SaveToCloud"

StyledDialogView {
    id: root

    title: prv.isExport ? qsTrc("project/export", "Export audio") : qsTrc("project/save", "Save project")

    contentHeight: 600
    contentWidth: 900

    objectName: "AskLocationTypeDialog"

    property bool askAgain: true
    property string purpose: "save"

    QtObject {
        id: prv

        readonly property bool isExport: root.purpose === "export"
    }

    function done(locationType) {
        root.ret = {
            errcode: 0,
            value: {
                askAgain: root.askAgain,
                locationType: locationType
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
            text: prv.isExport ? qsTrc("project/export", "How would you like to export?") : qsTrc("project/save", "How would you like to save?")
            font: ui.theme.headerBoldFont
        }

        RowLayout {
            id: optionsRowLayout
            spacing: 24

            NavigationPanel {
                id: optionsNavPanel
                name: "LocationOptionsButtons"
                enabled: optionsRowLayout.enabled && optionsRowLayout.visible
                direction: NavigationPanel.Horizontal
                section: root.navigationSection
                order: 1
            }

            SaveLocationOption {
                title: prv.isExport ? qsTrc("project/export", "Share to audio.com") : qsTrc("project/save", "Save to the cloud (free)")
                description: prv.isExport ? qsTrc("project/export", "Uploads an uncompressed audio file and generates a shareable link. This link allows others to download the file in either .wav or .mp3 format.") : qsTrc("project/save", "Your project is backed up privately on audio.com. You can access your work from any device and collaborate on your project with others. Cloud saving is free for a limited number of projects.")
                buttonText: prv.isExport ? qsTrc("project/export", "Share to audio.com") : qsTrc("project/save", "Save to cloud")

                imageSource: "qrc:/SaveToCloud/images/Cloud.png"

                navigation.panel: optionsNavPanel
                navigation.column: 1
                navigation.accessible.name: title
                navigation.accessible.description: description

                onButtonClicked: {
                    root.done(SaveLocationType.Cloud)
                }
            }

            SaveLocationOption {
                title: qsTrc("project/save", "On your computer")
                description: prv.isExport ? qsTrc("project/export", "Export MP3s, WAVs, FLACs and other formats to your computer.") : qsTrc("project/save", "If you prefer to save your files on your computer, you can do that here.")
                buttonText: prv.isExport ? qsTrc("project/export", "Export to computer") : qsTrc("project/save", "Save to computer")

                imageSource: "qrc:/SaveToCloud/images/Laptop.png"

                navigation.panel: optionsNavPanel
                navigation.column: 2
                navigation.accessible.name: title
                navigation.accessible.description: description

                onButtonClicked: {
                    root.done(SaveLocationType.Local)
                }
            }
        }

        SeparatorLine {
            Layout.margins: -24
        }

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
