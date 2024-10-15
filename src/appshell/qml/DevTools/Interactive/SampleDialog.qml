/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import Muse.Ui 1.0
import Muse.UiComponents 1.0

StyledDialogView {

    id: root

    property color color: "#444444"
    property bool isApplyColor: false

    contentWidth: 400
    contentHeight: 400

    title: "Sample dialog"

    Rectangle {
        anchors.fill: parent
        color: root.isApplyColor ? root.color : "#666666"

        Column {
            anchors.centerIn: parent

            spacing: 50

            TextInputField {
                id: input
                anchors.horizontalCenter: parent.horizontalCenter

                property string value: ""
                width: 150
                onTextChanged: input.value = newTextValue
            }

            StyledTextLabel {
                anchors.horizontalCenter: parent.horizontalCenter

                text: "Use right click for showing context menu"
            }
        }

        Row {
            anchors.bottom: parent.bottom
            anchors.right: parent.right

            anchors.rightMargin: 16
            anchors.bottomMargin: 20
            spacing: 20

            FlatButton {
                text: "Cancel"
                onClicked: {
                    root.reject()
                }
            }

            FlatButton {
                text: "OK"
                onClicked: {
                    root.ret = {errcode: 0, value: input.value }
                    root.hide()
                }
            }
        }

        MouseArea {
            anchors.fill: parent
            acceptedButtons: Qt.RightButton
            onClicked: function(mouse) {
                var items = [
                            {id: "undo", title: "Undo", shortcut: "Ctrl+Z", icon: IconCode.UNDO},
                            {id: "redo", title: "Redo", shortcut: "Shift+Ctrl+Z", enabled: false, icon: IconCode.REDO},
                            {},
                            {id: "zoomin", title: "Zoom in", icon: IconCode.ZOOM_IN},
                            {id: "zoomout", title: "Zoom out", icon: IconCode.ZOOM_OUT},
                            {},
                            {id: "checkable", title: "Checkable", checkable: true, checked: false}
                        ]

                menuLoader.toggleOpened(items, mouse.x, mouse.y)
            }
        }

        StyledMenuLoader {
            id: menuLoader

            onHandleMenuItem: function(itemId) {
                console.log("selected " + itemId)
            }
        }
    }
}
