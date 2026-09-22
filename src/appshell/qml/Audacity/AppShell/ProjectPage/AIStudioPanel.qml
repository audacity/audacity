/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents
import Audacity.AIStudio

Item {
    id: root

    required property var navigationSection
    required property int navigationOrderStart

    function importWav() {
        const sourcePaths = wavPicker.selectFiles()
        for (let index = 0; index < sourcePaths.length; ++index) {
            AIStudioStatus.importLocalWav(sourcePaths[index])
        }
    }

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 16
        spacing: 12

        StyledTextLabel {
            Layout.fillWidth: true
            text: qsTrc("aistudio", "AI Studio")
            font: ui.theme.headerBoldFont
        }

        StyledTextLabel {
            Layout.fillWidth: true
            text: AIStudioStatus.runtimeStatus
            font: ui.theme.bodyBoldFont
        }

        StyledTextLabel {
            Layout.fillWidth: true
            wrapMode: Text.Wrap
            text: AIStudioStatus.workspaceStatus
        }

        StyledTextLabel {
            Layout.fillWidth: true
            wrapMode: Text.Wrap
            text: qsTrc("aistudio", "Run a deterministic local provider job to verify the authenticated job boundary. It writes a disposable WAV and manifest only; it does not alter this project.")
        }

        FlatButton {
            text: qsTrc("aistudio", "Enable project AI workspace")
            onClicked: AIStudioStatus.enableProjectWorkspace()
        }

        Rectangle {
            Layout.fillWidth: true
            Layout.preferredHeight: 1
            color: ui.theme.strokeColor
        }

        StyledTextLabel {
            Layout.fillWidth: true
            text: qsTrc("aistudio", "Library — This Project")
            font: ui.theme.bodyBoldFont
        }

        FilePickerModel {
            id: wavPicker
            title: qsTrc("aistudio", "Import WAV files into AI Library")
            filter: [qsTrc("aistudio", "WAV audio (*.wav)")]
        }

        RowLayout {
            Layout.fillWidth: true
            spacing: 8

            FlatButton {
                text: qsTrc("aistudio", "Open Library")
                onClicked: libraryBrowser.open()
            }

            FlatButton {
                text: qsTrc("aistudio", "Import local WAV")
                onClicked: root.importWav()
            }
        }

        StyledTextLabel {
            Layout.fillWidth: true
            wrapMode: Text.Wrap
            text: qsTrc("aistudio", "Library activity: %1").arg(AIStudioStatus.libraryStatus)
            font: ui.theme.bodyBoldFont
        }

        ListView {
            Layout.fillWidth: true
            Layout.preferredHeight: Math.min(contentHeight, 132)
            clip: true
            spacing: 6
            model: AIStudioStatus.libraryAssets
            delegate: Column {
                width: ListView.view.width
                spacing: 2

                StyledTextLabel {
                    width: parent.width
                    elide: Text.ElideRight
                    text: modelData.name
                    font: ui.theme.bodyBoldFont
                }
                StyledTextLabel {
                    width: parent.width
                    elide: Text.ElideRight
                    text: modelData.origin + " · " + modelData.status
                    font: ui.theme.bodyFont
                }
                FlatButton {
                    text: qsTrc("aistudio", "Add to Timeline")
                    enabled: modelData.status === "available"
                    onClicked: AIStudioStatus.addLibraryAssetToTimeline(modelData.id)
                }
            }
        }

        StyledTextLabel {
            Layout.fillWidth: true
            visible: AIStudioStatus.libraryAssets.length === 0
            wrapMode: Text.Wrap
            text: qsTrc("aistudio", "Imported and generated assets stay here even when they are not on the timeline.")
        }

        FlatButton {
            text: qsTrc("aistudio", "Run test provider job")
            enabled: AIStudioStatus.runtimeStatus === qsTrc("aistudio", "Runtime host healthy")
            onClicked: AIStudioStatus.runTestJob()
        }

        FlatButton {
            text: qsTrc("aistudio", "Cancel test provider job")
            enabled: AIStudioStatus.runtimeStatus === qsTrc("aistudio", "Test provider job running")
            onClicked: AIStudioStatus.cancelTestJob()
        }

        FlatButton {
            text: qsTrc("aistudio", "Insert completed test output")
            onClicked: AIStudioStatus.insertTestJobOutput()
        }

        FlatButton {
            text: qsTrc("aistudio", "Run worker-failure test")
            enabled: AIStudioStatus.runtimeStatus === qsTrc("aistudio", "Runtime host healthy")
            onClicked: AIStudioStatus.runWorkerFailureTest()
        }

        Rectangle {
            Layout.fillWidth: true
            Layout.preferredHeight: 1
            color: ui.theme.strokeColor
        }

        StyledTextLabel {
            Layout.fillWidth: true
            wrapMode: Text.Wrap
            text: qsTrc("aistudio", "Planned: Create, Plan, Separate, Vocals, Instruments, and Jobs.")
        }

        Item { Layout.fillHeight: true }
    }

    Popup {
        id: libraryBrowser

        parent: Overlay.overlay
        anchors.centerIn: parent
        width: 800
        height: 500
        modal: true
        focus: true
        padding: 0
        closePolicy: Popup.CloseOnEscape | Popup.CloseOnPressOutside

        property string filterKind: "all"
        property string selectedAssetId: ""
        property var displayedAssets: {
            const assets = AIStudioStatus.libraryAssets
            if (filterKind === "all") {
                return assets
            }
            return assets.filter(function(asset) {
                return filterKind === "imports" ? asset.origin === "uploaded" : asset.origin === "generated"
            })
        }
        property var selectedAsset: {
            for (let index = 0; index < displayedAssets.length; ++index) {
                if (displayedAssets[index].id === selectedAssetId) {
                    return displayedAssets[index]
                }
            }
            return null
        }

        background: Rectangle {
            color: ui.theme.backgroundPrimaryColor
            border.color: ui.theme.strokeColor
            border.width: 1
            radius: 4
        }

        ColumnLayout {
            anchors.fill: parent
            spacing: 0

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: 54
                color: ui.theme.backgroundSecondaryColor

                RowLayout {
                    anchors.fill: parent
                    anchors.leftMargin: 18
                    anchors.rightMargin: 10
                    spacing: 10

                    StyledTextLabel {
                        text: qsTrc("aistudio", "Library — This Project")
                        font: ui.theme.headerBoldFont
                    }

                    Item { Layout.fillWidth: true }

                    FlatButton {
                        text: qsTrc("aistudio", "Import WAV")
                        onClicked: root.importWav()
                    }

                    FlatButton {
                        text: qsTrc("global", "Close")
                        onClicked: libraryBrowser.close()
                    }
                }
            }

            StyledTextLabel {
                Layout.fillWidth: true
                Layout.leftMargin: 18
                Layout.rightMargin: 18
                Layout.topMargin: 10
                Layout.bottomMargin: 10
                text: qsTrc("aistudio", "This Project  /  AI Library")
                font: ui.theme.bodyBoldFont
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.fillHeight: true
                color: ui.theme.backgroundPrimaryColor

                RowLayout {
                    anchors.fill: parent
                    spacing: 0

                    Rectangle {
                        Layout.fillHeight: true
                        Layout.preferredWidth: 150
                        color: ui.theme.backgroundSecondaryColor

                        ColumnLayout {
                            anchors.fill: parent
                            anchors.margins: 10
                            spacing: 4

                            StyledTextLabel {
                                Layout.fillWidth: true
                                text: qsTrc("aistudio", "Folders")
                                font: ui.theme.bodyBoldFont
                            }

                            Repeater {
                                model: [
                                    { key: "all", label: qsTrc("aistudio", "All assets") },
                                    { key: "imports", label: qsTrc("aistudio", "Imported WAV") },
                                    { key: "generated", label: qsTrc("aistudio", "Generated") }
                                ]

                                delegate: FlatButton {
                                    Layout.fillWidth: true
                                    text: modelData.label
                                    enabled: libraryBrowser.filterKind !== modelData.key
                                    onClicked: {
                                        libraryBrowser.filterKind = modelData.key
                                        libraryBrowser.selectedAssetId = ""
                                    }
                                }
                            }

                            Item { Layout.fillHeight: true }
                        }
                    }

                    Rectangle {
                        Layout.fillHeight: true
                        Layout.preferredWidth: 1
                        color: ui.theme.strokeColor
                    }

                    ColumnLayout {
                        Layout.fillWidth: true
                        Layout.fillHeight: true
                        Layout.margins: 12
                        spacing: 6

                        RowLayout {
                            Layout.fillWidth: true

                            StyledTextLabel { Layout.fillWidth: true; text: qsTrc("aistudio", "Name"); font: ui.theme.bodyBoldFont }
                            StyledTextLabel { Layout.preferredWidth: 82; text: qsTrc("aistudio", "Type"); font: ui.theme.bodyBoldFont }
                            StyledTextLabel { Layout.preferredWidth: 82; text: qsTrc("aistudio", "Status"); font: ui.theme.bodyBoldFont }
                        }

                        Rectangle { Layout.fillWidth: true; Layout.preferredHeight: 1; color: ui.theme.strokeColor }

                        ListView {
                            id: assetList
                            Layout.fillWidth: true
                            Layout.fillHeight: true
                            clip: true
                            spacing: 2
                            model: libraryBrowser.displayedAssets

                            delegate: Rectangle {
                                required property var modelData
                                width: ListView.view.width
                                height: 38
                                color: libraryBrowser.selectedAssetId === modelData.id ? ui.theme.accentColor : "transparent"
                                radius: 3

                                RowLayout {
                                    anchors.fill: parent
                                    anchors.leftMargin: 8
                                    anchors.rightMargin: 8
                                    spacing: 8

                                    StyledTextLabel { Layout.fillWidth: true; elide: Text.ElideRight; text: modelData.name }
                                    StyledTextLabel { Layout.preferredWidth: 82; elide: Text.ElideRight; text: modelData.kind }
                                    StyledTextLabel { Layout.preferredWidth: 82; elide: Text.ElideRight; text: modelData.status }
                                }

                                MouseArea {
                                    anchors.fill: parent
                                    onClicked: libraryBrowser.selectedAssetId = modelData.id
                                }
                            }

                            StyledTextLabel {
                                anchors.centerIn: parent
                                visible: assetList.count === 0
                                text: qsTrc("aistudio", "No assets in this folder")
                            }
                        }

                        Rectangle { Layout.fillWidth: true; Layout.preferredHeight: 1; color: ui.theme.strokeColor }

                        RowLayout {
                            Layout.fillWidth: true

                            StyledTextLabel {
                                Layout.fillWidth: true
                                elide: Text.ElideRight
                                text: libraryBrowser.selectedAsset
                                      ? qsTrc("aistudio", "%1  ·  %2  ·  %3").arg(libraryBrowser.selectedAsset.name).arg(libraryBrowser.selectedAsset.origin).arg(libraryBrowser.selectedAsset.status)
                                      : qsTrc("aistudio", "Select an asset to view its details")
                            }

                            FlatButton {
                                text: qsTrc("aistudio", "Add to Timeline")
                                enabled: libraryBrowser.selectedAsset && libraryBrowser.selectedAsset.status === "available"
                                onClicked: AIStudioStatus.addLibraryAssetToTimeline(libraryBrowser.selectedAsset.id)
                            }
                        }
                    }
                }
            }
        }
    }
}
