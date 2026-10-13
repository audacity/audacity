/*
* Audacity: A Digital Audio Editor
*/
pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.MacroManager

import "internal"

StyledDialogView {
    id: root

    property string macroName: ""

    title: qsTrc("macromanager", "Edit macro")

    contentWidth: prv.initialWidth
    contentHeight: prv.height
    resizable: true
    margins: 0

    QtObject {
        id: prv

        readonly property int initialWidth: 720
        readonly property int minimumWidth: 560
        readonly property int height: 480
        readonly property int leftColumnWidth: 280
        readonly property int rightColumnWidth: 440
        readonly property int rowHeight: 44
        readonly property int sideMargin: 8
        readonly property int buttonsSpacing: 8
        readonly property int buttonHeight: 28
    }

    onIsOpenedChanged: {
        if (root.isOpened) {
            root.contentWidth = prv.minimumWidth
        }
    }

    AvailableCommandsModel {
        id: availableCommandsModel
    }

    MacroCommandsModel {
        id: macroCommandsModel
    }

    Component.onCompleted: {
        availableCommandsModel.load()
    }

    onNavigationActivateRequested: {
        searchBar.navigationPanel.requestActive()
    }

    ColumnLayout {
        anchors.fill: parent

        spacing: 0

        RowLayout {
            Layout.fillWidth: true
            Layout.fillHeight: true

            spacing: 0

            ColumnLayout {
                Layout.preferredWidth: prv.leftColumnWidth
                Layout.fillWidth: true
                Layout.fillHeight: true

                spacing: 0

                CommandsSearchBar {
                    id: searchBar

                    Layout.fillWidth: true
                    Layout.preferredHeight: prv.rowHeight

                    searchText: availableCommandsModel.searchText

                    navigationPanel.section: root.navigationSection
                    navigationPanel.order: 1

                    onSearchTextChangeRequested: function (text) {
                        availableCommandsModel.setSearchText(text)
                    }
                }

                SeparatorLine {}

                AvailableCommandsList {
                    Layout.fillWidth: true
                    Layout.fillHeight: true

                    model: availableCommandsModel

                    navigation.section: root.navigationSection
                    navigation.order: 2

                    onAddRequested: function (code, title) {
                        macroCommandsModel.add(code, title)
                    }
                }
            }

            SeparatorLine {}

            Rectangle {
                Layout.preferredWidth: prv.rightColumnWidth
                Layout.fillWidth: true
                Layout.fillHeight: true

                color: ui.theme.backgroundTertiaryColor

                ColumnLayout {
                    anchors.fill: parent

                    spacing: 0

                    Rectangle {
                        Layout.fillWidth: true
                        Layout.preferredHeight: prv.rowHeight

                        color: ui.theme.backgroundPrimaryColor

                        StyledTextLabel {
                            anchors.fill: parent
                            anchors.leftMargin: prv.sideMargin
                            anchors.rightMargin: prv.sideMargin

                            text: root.macroName
                            font: ui.theme.bodyBoldFont
                            horizontalAlignment: Qt.AlignLeft
                            elide: Text.ElideRight
                        }
                    }

                    SeparatorLine {}

                    MacroCommandsTable {
                        Layout.fillWidth: true
                        Layout.fillHeight: true

                        model: macroCommandsModel

                        navigationPanel.section: root.navigationSection
                        navigationPanel.order: 3

                        onDeleteRequested: function (row) {
                            macroCommandsModel.remove(row)
                        }
                    }
                }
            }
        }

        SeparatorLine {}

        RowLayout {
            Layout.fillWidth: true
            Layout.preferredHeight: prv.rowHeight
            Layout.leftMargin: prv.sideMargin
            Layout.rightMargin: prv.sideMargin

            spacing: prv.buttonsSpacing

            property NavigationPanel navigationPanel: NavigationPanel {
                name: "EditMacroButtons"
                direction: NavigationPanel.Horizontal
                section: root.navigationSection
                order: 4
            }

            FlatButton {
                id: runButton

                Layout.preferredHeight: prv.buttonHeight

                text: qsTrc("macromanager", "Run")
                enabled: false

                navigation.panel: parent.navigationPanel
                navigation.column: 0
            }

            FlatButton {
                Layout.preferredHeight: prv.buttonHeight

                text: qsTrc("macromanager", "Run on files")
                enabled: false

                navigation.panel: parent.navigationPanel
                navigation.column: 1
            }

            Item {
                Layout.fillWidth: true
            }

            FlatButton {
                Layout.preferredHeight: prv.buttonHeight

                text: qsTrc("macromanager", "Done")

                navigation.panel: parent.navigationPanel
                navigation.column: 2

                onClicked: {
                    root.hide()
                }
            }
        }
    }
}
