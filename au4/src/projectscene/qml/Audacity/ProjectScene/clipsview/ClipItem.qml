import QtQuick

import Muse.Ui
import Muse.UiComponents
import Muse.GraphicalEffects

import Audacity.ProjectScene

RoundedRectangle {

    id: root

    property alias context: waveView.context
    property alias clipKey: waveView.clipKey
    property alias title: titleLabel.text
    property color clipColor: "#677CE4"
    property bool clipSelected: false

    property bool collapsed: false

    signal positionChanged(x : double)
    signal requestSelected()
    signal titleEdited(var newTitle)

    radius: 4
    color: "#000000" // border color

    Drag.active: headerDragArea.drag.active

    property int borderWidth: 1
    property bool hover: hoverArea.containsMouse || headerDragArea.containsMouse


    MouseArea {
        id: hoverArea
        anchors.fill: parent
        hoverEnabled: root.collapsed
    }

    Item {
        id: inner

        anchors.fill: parent
        anchors.margins: root.borderWidth

        layer.enabled: true
        layer.effect: EffectOpacityMask {
            maskSource: RoundedRectangle {
                width: inner.width
                height: inner.height
                radius: root.radius
            }
        }

        Rectangle {
            id: header
            anchors.top: parent.top
            anchors.left: parent.left
            anchors.right: parent.right

            height: 20
            z: 2

            visible: !root.collapsed || root.hover

            MouseArea {
                id: headerDragArea
                anchors.fill: parent

                hoverEnabled: true
                cursorShape: Qt.OpenHandCursor
                drag.target: root
                drag.axis: Drag.XAxis

                onReleased: {
                    if (drag.active) {
                        root.positionChanged(root.x)
                    }
                }

                onClicked: root.requestSelected()

                onDoubleClicked: {
                    titleEdit.visible = true
                }
            }

            StyledTextLabel {
                id: titleLabel
                anchors.top: parent.top
                anchors.bottom: parent.bottom
                anchors.left: parent.left
                anchors.right: menuBtn.left
                anchors.leftMargin: 4
                anchors.rightMargin: 2
                horizontalAlignment: Qt.AlignLeft
            }

            TextInputField {
                id: titleEdit
                anchors.fill: titleLabel
                currentText: titleLabel.text
                visible: false
                onTextEditingFinished: function (newTitle) {
                    titleEdit.visible = false
                    root.titleEdited(newTitle)
                }
            }

            MenuButton {
                id: menuBtn
                width: 16
                height: 16
                anchors.right: parent.right
                anchors.rightMargin: 4
                anchors.verticalCenter: parent.verticalCenter

                menuModel: [
                    {"id": 1, "title": "Item 1"},
                    {"id": 2, "title": "Item 2"}
                ]

                onHandleMenuItem: function(itemId) {
                    console.log("onHandleMenuItem: " + itemId)
                }
            }
        }

        WaveView {
            id: waveView
            anchors.top: (!root.collapsed && header.visible) ? header.bottom : parent.top
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.bottom: parent.bottom

            clipColor: root.clipColor
            clipLeft: root.x
            clipSelected: root.clipSelected
        }
    }

    state: "NORMAL"
    states: [
        State {
            name: "NORMAL"
            when: !root.clipSelected && !headerDragArea.containsMouse
            PropertyChanges { target: header; color: root.clipColor }
            PropertyChanges { target: titleLabel; color: "#ffffff"}
            //PropertyChanges { target: menuBtn; iconColor: "#ffffff"}
        },

        State {
            name: "SELECTED"
            when: root.clipSelected && !headerDragArea.containsMouse
            PropertyChanges { target: header; color: ui.blendColors("#ffffff", root.clipColor, 0.3) }
            PropertyChanges { target: titleLabel; color: "#000000" }
            //PropertyChanges { target: menuBtn; iconColor: "#000000"}
        },

        State {
            name: "NORMAL_HEADER_HOVERED"
            when: !root.clipSelected && headerDragArea.containsMouse
            PropertyChanges { target: header; color: ui.blendColors("#ffffff", root.clipColor, 0.8)}
            PropertyChanges { target: titleLabel; color: "#ffffff"}
            //PropertyChanges { target: menuBtn; iconColor: "#ffffff"}
        },

        State {
            name: "SELECTED_HEADER_HOVERED"
            when: root.clipSelected && headerDragArea.containsMouse
            PropertyChanges { target: header; color: ui.blendColors("#ffffff", root.clipColor, 0.2) }
            PropertyChanges { target: titleLabel; color: "#000000"}
            //PropertyChanges { target: menuBtn; iconColor: "#000000"}
        }
    ]
}
