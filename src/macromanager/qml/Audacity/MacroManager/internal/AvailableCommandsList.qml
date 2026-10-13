/*
* Audacity: A Digital Audio Editor
*/
pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

StyledListView {
    id: root

    signal addRequested(string code, string title)

    QtObject {
        id: prv

        readonly property int rowHeight: 32
        readonly property int sideMargin: 8
        readonly property int addButtonSize: 24
        readonly property int titleButtonSpacing: 10
    }

    currentIndex: -1

    navigation.name: "AvailableCommandsList"
    navigation.direction: NavigationPanel.Vertical
    navigation.accessible.name: qsTrc("macromanager", "Available commands")

    delegate: ListItemBlank {
        id: item

        required property int index
        required property string code
        required property string title

        width: ListView.view.width
        height: prv.rowHeight

        isSelected: ListView.isCurrentItem

        navigation.panel: root.navigation
        navigation.row: item.index
        navigation.accessible.name: item.title

        onClicked: {
            root.currentIndex = item.index
        }

        onDoubleClicked: {
            root.addRequested(item.code, item.title)
        }

        RowLayout {
            anchors.fill: parent
            anchors.leftMargin: prv.sideMargin
            anchors.rightMargin: prv.sideMargin

            spacing: prv.titleButtonSpacing

            StyledTextLabel {
                Layout.fillWidth: true
                Layout.fillHeight: true

                text: item.title
                horizontalAlignment: Qt.AlignLeft
                elide: Text.ElideRight
            }

            FlatButton {
                Layout.preferredWidth: prv.addButtonSize
                Layout.preferredHeight: prv.addButtonSize

                visible: item.isSelected

                icon: IconCode.PLUS
                transparent: true

                navigation.panel: root.navigation
                navigation.row: item.index
                navigation.column: 1
                navigation.name: "AddCommand"
                navigation.accessible.name: qsTrc("macromanager", "Add command")

                onClicked: {
                    root.addRequested(item.code, item.title)
                }
            }
        }
    }
}
