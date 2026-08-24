/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents
import Audacity.Project 1.0

import "internal/SaveToCloud"

StyledDialogView {
    id: root

    title: qsTrc("project/export", "Export audio")

    contentHeight: 600
    contentWidth: 900

    objectName: "AskExportLocationTypeDialog"

    property bool askAgain: true

    function done(exportLocationType) {
        root.ret = {
            errcode: 0,
            value: {
                askAgain: root.askAgain,
                exportLocationType: exportLocationType
            }
        };

        root.hide();
    }

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 24
        spacing: 24

        StyledTextLabel {
            Layout.fillWidth: true
            text: qsTrc("project/export", "How would you like to export?")
            font: ui.theme.headerBoldFont
        }

        RowLayout {
            id: optionsRowLayout
            spacing: 24

            NavigationPanel {
                id: optionsNavPanel
                name: "ExportLocationOptionsButtons"
                enabled: optionsRowLayout.enabled && optionsRowLayout.visible
                direction: NavigationPanel.Horizontal
                section: root.navigationSection
                order: 1
            }

            SaveLocationOption {
                title: qsTrc("project/export", "Share to audio.com")
                description: qsTrc("project/export", "Uploads an uncompressed audio file and generates a shareable link. This link allows others to download the file in either .wav or .mp3 format.")
                buttonText: qsTrc("project/export", "Share to audio.com")

                imageSource: "qrc:/SaveToCloud/images/Cloud.png"

                navigation.panel: optionsNavPanel
                navigation.column: 1
                navigation.accessible.name: qsTrc("project/export", "Share to audio.com")
                navigation.accessible.description: description

                onButtonClicked: {
                    root.done(SaveLocationType.Cloud);
                }
            }

            SaveLocationOption {
                title: qsTrc("project/export", "On your computer")
                description: qsTrc("project/export", "Export MP3s, WAVs, FLACs and other formats to your computer.")
                buttonText: qsTrc("project/export", "Export to computer")

                imageSource: "qrc:/SaveToCloud/images/Laptop.png"

                navigation.panel: optionsNavPanel
                navigation.column: 2
                navigation.accessible.name: qsTrc("project/export", "Export on your computer")
                navigation.accessible.description: description

                onButtonClicked: {
                    root.done(SaveLocationType.Local);
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
                root.askAgain = !root.askAgain;
            }
        }
    }
}
