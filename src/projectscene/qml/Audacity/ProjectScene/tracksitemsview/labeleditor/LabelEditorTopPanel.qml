/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents

Item {
    id: root

    property alias canRemove: deleteButton.enabled

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "LabelEditorTopPanel"
        direction: NavigationPanel.Horizontal
        accessible.role: MUAccessible.Dialog
        accessible.name: titleLabel.text
    }

    implicitHeight: 48

    signal importRequested()
    signal exportRequested()
    signal removeSelectedLabelsRequested()
    signal addLabelRequested()

    function readInfo() {
        accessibleInfo.ignored = false
        accessibleInfo.focused = true
    }

    AccessibleItem {
        id: accessibleInfo
        visualItem: root
        role: MUAccessible.Button
        name: titleLabel.text // todo
    }

    RowLayout {
        anchors.verticalCenter: parent.verticalCenter
        anchors.left: parent.left
        anchors.leftMargin: 16
        anchors.right: parent.right
        anchors.rightMargin: 12

        spacing: 8

        StyledTextLabel {
            id: titleLabel
            Layout.alignment: Qt.AlignLeft

            text: qsTrc("projectscene", "Labels")
            font: ui.theme.largeBodyBoldFont
        }

        Item {
            Layout.fillWidth: true
        }

        FlatButton {
            Layout.alignment: Qt.AlignRight

            text: qsTrc("projectscene", "Import")
            isNarrow: true

            navigation.name: "ImportButton"
            navigation.panel: root.navigationPanel
            navigation.column: 1

            onClicked: {
                root.importRequested()
            }
        }


        FlatButton {
            Layout.alignment: Qt.AlignRight

            text: qsTrc("projectscene", "Export")
            isNarrow: true

            navigation.name: "ExportButton"
            navigation.panel: root.navigationPanel
            navigation.column: 2

            onClicked: {
                root.exportRequested()
            }
        }

        SeparatorLine {}

        FlatButton {
            id: deleteButton

            Layout.alignment: Qt.AlignRight

            text: qsTrc("projectscene", "Delete")
            isNarrow: true

            navigation.name: "DeleteButton"
            navigation.panel: root.navigationPanel
            navigation.column: 3

            onClicked: {
                root.removeSelectedLabelsRequested()
            }
        }

        FlatButton {
            id: addButton

            Layout.alignment: Qt.AlignRight

            text: qsTrc("projectscene", "Add label")
            isNarrow: true

            navigation.name: "AddButton"
            navigation.panel: root.navigationPanel
            navigation.column: 4

            onClicked: {
                root.addLabelRequested()
            }
        }
    }
}
