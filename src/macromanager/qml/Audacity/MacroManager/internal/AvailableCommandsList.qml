/*
* Audacity: A Digital Audio Editor
*/
pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

StyledListView {
    id: root

    property string dragMimeType: ""

    signal addRequested(string code, string title, bool isMacro)

    QtObject {
        id: prv

        readonly property int rowHeight: 32
        readonly property int sideMargin: 8
        readonly property int addButtonSize: 24
        readonly property int titleButtonSpacing: 10
    }

    navigation.name: "AvailableCommandsList"
    navigation.direction: NavigationPanel.Vertical
    navigation.accessible.name: qsTrc("macromanager", "Available commands")

    scrollBarPolicy: ScrollBar.AlwaysOn

    delegate: ListItemBlank {
        id: item

        required property int index
        required property string code
        required property string title
        required property bool isMacro

        width: ListView.view.width
        height: prv.rowHeight

        isSelected: item.navigation.active

        navigation.panel: root.navigation
        navigation.row: item.index
        navigation.accessible.name: item.title

        mouseArea.drag.target: dragTarget

        Drag.dragType: Drag.Automatic
        Drag.supportedActions: Qt.CopyAction
        Drag.active: item.mouseArea.drag.active
        Drag.mimeData: {
            const data = {}
            data[root.dragMimeType] = JSON.stringify({
                row: -1,
                code: item.code,
                title: item.title,
                isMacro: item.isMacro
            })
            return data
        }

        onClicked: function (mouse) {
            if (!mouse) {
                root.addRequested(item.code, item.title, item.isMacro)
            }
        }

        onDoubleClicked: {
            root.addRequested(item.code, item.title, item.isMacro)
        }

        onPressed: function (isPressed) {
            if (isPressed) {
                item.grabToImage(function (result) {
                    item.Drag.imageSource = result.url
                })
            }
        }

        Item {
            id: dragTarget
        }

        RowLayout {
            anchors.fill: parent
            anchors.leftMargin: prv.sideMargin
            anchors.rightMargin: prv.sideMargin

            spacing: prv.titleButtonSpacing

            StyledIconLabel {
                visible: item.isMacro

                iconCode: IconCode.ACCOUNT
            }

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

                onClicked: {
                    root.addRequested(item.code, item.title, item.isMacro)
                }
            }
        }
    }
}
