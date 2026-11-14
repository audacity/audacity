/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.UiComponents 1.0

BaseSection {
    id: root

    property var paths: []
    property string dialogTitle: qsTrc("preferences", "Choose plugin location")
    property var pathValidator: function (path) {
        return true
    }

    signal addPathRequested
    signal pathChanged(int index, string newPath)
    signal removePathRequested(int index)

    readonly property bool hasEmptyRow: {
        for (let i = 0; i < paths.length; ++i) {
            if (!paths[i] || paths[i].length === 0) {
                return true
            }
        }
        return false
    }

    navigation.direction: NavigationPanel.Both

    Column {
        width: parent.width
        spacing: 8

        Repeater {
            model: root.paths

            delegate: Column {
                width: parent.width
                spacing: 4

                property int rowIndex: index
                property string currentPath: modelData
                readonly property bool pathInvalid: !root.pathValidator(currentPath)

                RowLayout {
                    width: parent.width
                    spacing: 4

                    FilePicker {
                        Layout.fillWidth: true

                        pickerType: FilePicker.PickerType.Directory
                        dialogTitle: root.dialogTitle
                        path: modelData

                        navigation: root.navigation
                        navigationRowOrderStart: rowIndex
                        navigationColumnOrderStart: 0
                        pathFieldTitle: root.title

                        onPathEdited: function (newPath) {
                            currentPath = newPath
                            root.pathChanged(rowIndex, newPath)
                        }
                    }

                    FlatButton {
                        Layout.alignment: Qt.AlignVCenter

                        icon: IconCode.DELETE_TANK
                        text: qsTrc("preferences", "Remove")
                        buttonType: FlatButton.IconOnly

                        accessible.name: qsTrc("preferences", "Remove location")

                        navigation.panel: root.navigation
                        navigation.row: rowIndex
                        navigation.column: 2

                        onClicked: {
                            root.removePathRequested(rowIndex)
                        }
                    }
                }

                StyledTextLabel {
                    width: parent.width
                    leftPadding: 12
                    horizontalAlignment: Text.AlignLeft

                    visible: pathInvalid
                    Accessible.ignored: !pathInvalid

                    text: qsTrc("preferences", "The directory does not exist")
                    color: ui.theme.extra["error_text_color"]

                    font: Qt.font(Object.assign({}, ui.theme.bodyFont, {
                        pointSize: ui.theme.bodyFont.pointSize - 1
                    }))
                }
            }
        }

        FlatButton {
            text: qsTrc("preferences", "Add new location")

            enabled: !root.hasEmptyRow

            navigation.panel: root.navigation
            navigation.row: root.paths.length
            navigation.column: 0

            onClicked: {
                root.addPathRequested()
            }
        }
    }
}
