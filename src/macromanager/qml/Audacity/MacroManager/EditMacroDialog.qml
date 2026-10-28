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
        readonly property int maxLeftColumnWidth: 400
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

        onMacroDeleted: {
            root.hide()
        }
    }

    Component.onCompleted: {
        availableCommandsModel.load(root.macroName)
        macroCommandsModel.load(root.macroName)
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
                Layout.maximumWidth: prv.maxLeftColumnWidth
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

                    onSearchAccepted: {
                        const count = availableCommandsModel.rowCount()
                        if (count === 0) {
                            return
                        }

                        if (count > 1) {
                            availableCommandsList.navigation.requestActive()
                            return
                        }

                        const command = availableCommandsModel.itemAt(0)
                        if (command.isMacro) {
                            macroCommandsModel.addMacro(command.code)
                        } else {
                            macroCommandsModel.add(command.code)
                        }
                    }
                }

                SeparatorLine {}

                AvailableCommandsList {
                    id: availableCommandsList

                    Layout.fillWidth: true
                    Layout.fillHeight: true

                    model: availableCommandsModel
                    dragMimeType: commandsTable.dragMimeType

                    navigation.section: root.navigationSection
                    navigation.order: 2

                    onAddRequested: function (code, title, isMacro) {
                        if (isMacro) {
                            macroCommandsModel.addMacro(code)
                        } else {
                            macroCommandsModel.add(code)
                        }
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

                    MacroTitleBar {
                        Layout.fillWidth: true
                        Layout.preferredHeight: prv.rowHeight

                        title: macroCommandsModel.name

                        navigationPanel.section: root.navigationSection
                        navigationPanel.order: 3

                        onRenameRequested: {
                            macroCommandsModel.rename()
                        }

                        onExportRequested: {
                            macroCommandsModel.exportMacro()
                        }

                        onRemoveAllStepsRequested: {
                            macroCommandsModel.removeAllSteps()
                        }

                        onDeleteRequested: {
                            macroCommandsModel.deleteMacro()
                        }
                    }

                    SeparatorLine {}

                    MacroCommandsTable {
                        id: commandsTable

                        Layout.fillWidth: true
                        Layout.fillHeight: true

                        model: macroCommandsModel

                        navigationPanel.section: root.navigationSection
                        navigationPanel.order: 4

                        onEditRequested: function (row) {
                            macroCommandsModel.editParams(row)
                        }

                        onDeleteRequested: function (row) {
                            macroCommandsModel.remove(row)
                        }

                        onMoveRequested: function (from, to) {
                            macroCommandsModel.move(from, to)
                        }

                        onInsertRequested: function (row, code, title, isMacro) {
                            if (isMacro) {
                                macroCommandsModel.insertMacro(row, code)
                            } else {
                                macroCommandsModel.insert(row, code)
                            }
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
                order: 5
            }

            FlatButton {
                id: runButton

                Layout.preferredHeight: prv.buttonHeight

                text: qsTrc("macromanager", "Run")
                enabled: !macroCommandsModel.isEmpty && !macroCommandsModel.isRunning

                navigation.panel: parent.navigationPanel
                navigation.column: 0

                onClicked: {
                    macroCommandsModel.run()
                }
            }

            FlatButton {
                Layout.preferredHeight: prv.buttonHeight

                text: qsTrc("macromanager", "Run on files…")
                enabled: !macroCommandsModel.isEmpty && !macroCommandsModel.isRunning

                navigation.panel: parent.navigationPanel
                navigation.column: 1

                onClicked: {
                    macroCommandsModel.runOnFiles()
                }
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
                    if (macroCommandsModel.save()) {
                        root.hide()
                    }
                }
            }
        }
    }
}
