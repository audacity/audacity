import QtQuick

import Muse.Ui
import Muse.UiComponents
import Muse.GraphicalEffects

import Audacity.ProjectScene

Rectangle {

    id: root

    property alias context: waveView.context
    property alias clipKey: waveView.clipKey
    property alias clipTime: waveView.clipTime
    property alias title: titleLabel.text
    property alias showChannelSplitter: channelSplitter.visible
    property alias channelHeightRatio: channelSplitter.channelHeightRatio
    property var canvas: null
    property color clipColor: "#677CE4"
    property bool clipSelected: false

    property real leftVisibleMargin: 0
    property real rightVisibleMargin: 0

    property bool collapsed: false

    signal clipMoved(real deltaX, bool completed)
    signal clipLeftTrimmed(real deltaX, real posOnCanvas)
    signal clipRightTrimmed(real deltaX, real posOnCanvas)
    signal requestSelected()
    signal ratioChanged(double val)

    signal titleEditStarted()
    signal titleEditAccepted(var newTitle)
    signal titleEditCanceled()

    // mouse position event is not propagated on overlapping mouse areas
    // so we are handling it manually
    signal clipItemMousePositionChanged(real x, real y)

    radius: 4
    color: clipSelected ? "white" : clipColor
    border.color: "#000000"

    property int borderWidth: 1
    property bool hover: hoverArea.containsMouse || headerDragArea.containsMouse

    function editTitle() {
        editLoader.edit(titleLabel.text)
    }

    function acceptEditTitle(newTitle) {
        Qt.callLater(root.titleEditAccepted, newTitle)
    }

    ClipContextMenuModel {
        id: contextMenuModel
        clipKey: root.clipKey
    }

    Component.onCompleted: {
        contextMenuModel.load()
    }

    ContextMenuLoader {
        id: contextMenuLoader

        onHandleMenuItem: function(itemId) {
            contextMenuModel.handleMenuItem(itemId)
        }
    }

    MouseArea {
        id: hoverArea
        anchors.fill: parent
        acceptedButtons: Qt.RightButton
        hoverEnabled: true

        onClicked: function(e) {
            contextMenuLoader.show(Qt.point(e.x, e.y), contextMenuModel.items)
        }

        onPositionChanged: {
            clipItemMousePositionChanged(mouseX, mouseY)
        }
    }

    Rectangle {
        id: inner

        anchors.fill: parent
        anchors.margins: 1

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

                property bool moveActive: false
                property real moveLastX: 0.0

                onPositionChanged: function(e) {
                    root.clipItemMousePositionChanged(e.x, e.y)

                    if (headerDragArea.moveActive) {
                        var gx = mapToGlobal(e.x, e.y).x
                        root.clipMoved(gx - headerDragArea.moveLastX, false)
                        headerDragArea.moveLastX = gx
                    }
                }

                onPressed: function(e) {
                    root.requestSelected()

                    headerDragArea.moveLastX = mapToGlobal(e.x, e.y).x
                    headerDragArea.moveActive = true
                }

                onReleased: function(e) {
                    var gx = mapToGlobal(e.x, e.y).x
                    root.clipMoved(gx - headerDragArea.moveLastX, true)
                    headerDragArea.moveActive = false
                }

                onDoubleClicked: root.editTitle()
            }

            StyledTextLabel {
                id: titleLabel
                anchors.top: parent.top
                anchors.bottom: parent.bottom
                anchors.left: parent.left
                anchors.right: menuBtn.left
                anchors.leftMargin: root.leftVisibleMargin + 4
                anchors.rightMargin: 8
                horizontalAlignment: Qt.AlignLeft
            }

            Loader {
                id: editLoader

                anchors.fill: titleLabel

                property bool isEditState: false
                sourceComponent: editLoader.isEditState ? titleEditComp : null

                function edit(text) {
                    root.titleEditStarted()
                    editLoader.isEditState = true
                    editLoader.item.currentText = text
                    editLoader.item.newTitle = text
                    editLoader.item.visible = true
                    editLoader.item.ensureActiveFocus()
                }
            }

            Component {
                id: titleEditComp
                TextInputField {
                    id: titleEdit

                    property string newTitle: ""

                    anchors.fill: parent
                    background.color: header.color
                    background.border.width: 0
                    background.radius: 0
                    inputField.color: titleLabel.color
                    textSidePadding: 0
                    visible: false

                    onTextChanged: function(text) {
                        titleEdit.newTitle = text
                    }

                    onAccepted: {
                        root.acceptEditTitle(titleEdit.newTitle)
                        editLoader.isEditState = false
                    }

                    onEscaped: {
                        editLoader.isEditState = false
                    }

                    onFocusChanged: {
                        if (!titleEdit.focus) {
                            titleEdit.visible = false
                        }
                    }
                }
            }

            MenuButton {
                id: menuBtn
                width: 16
                height: 16
                anchors.right: parent.right
                anchors.rightMargin: root.rightVisibleMargin + 4
                anchors.verticalCenter: parent.verticalCenter

                menuModel: contextMenuModel

                onHandleMenuItem: function(itemId) {
                    contextMenuModel.handleMenuItem(itemId)
                }
            }
        }

        WaveView {
            id: waveView
            anchors.top: (!root.collapsed && header.visible) ? header.bottom : parent.top
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.bottom: parent.bottom

            channelHeightRatio: showChannelSplitter ? root.channelHeightRatio : 1

            clipColor: root.clipColor
            clipSelected: root.clipSelected

            ChannelSplitter {
                id: channelSplitter

                anchors.fill: parent

                color: "#000000"
                opacity: 0.10

                onRatioChanged: function (ratio) {
                    root.ratioChanged(ratio)
                }
            }
        }

        Rectangle {
            id: selectionHighlight
            anchors.fill: parent
            border.width: 1
            border.color: root.clipSelected ? "white" : "transparent"
            color: "transparent"
            radius: root.radius
            z: 2
        }
    }

    ClipHandles {
        id: clipHandles

        // +1 not to overlap with header
        y: header.height + 1
        width: root.width
        handlesVisible: root.clipSelected
        canvas: root.canvas


        // make sure clip handles are visible on top of nearby clips
        onHandlesVisibleChanged: {
            if (handlesVisible) {
                root.parent.z = 1
            } else {
                root.parent.z = 0
            }
        }

        onClipHandlesMousePositionChanged: function(xWithinClipHandles, yWithinClipHandles) {
            var xWithinClipItem = xWithinClipHandles
            var yWithinClipItem = header.height + 1 + yWithinClipHandles
            clipItemMousePositionChanged(xWithinClipItem, yWithinClipItem)
        }

        onTrimLeftBy: function(trimByX, posOnCanvas) {
            clipLeftTrimmed(trimByX, posOnCanvas)
        }

        onTrimRightBy: function(trimByX, posOnCanvas) {
            clipRightTrimmed(trimByX, posOnCanvas)
        }
    }

    state: "NORMAL"
    states: [
        State {
            name: "NORMAL"
            when: !root.clipSelected && !headerDragArea.containsMouse
            PropertyChanges { target: header; color: root.clipColor }
            PropertyChanges { target: titleLabel; color: "#ffffff"}
            PropertyChanges { target: menuBtn; iconColor: "#ffffff"}
        },

        State {
            name: "SELECTED"
            when: root.clipSelected && !headerDragArea.containsMouse
            PropertyChanges { target: header; color: ui.blendColors("#ffffff", root.clipColor, 0.3) }
            PropertyChanges { target: titleLabel; color: "#000000" }
            PropertyChanges { target: menuBtn; iconColor: "#000000"}
        },

        State {
            name: "NORMAL_HEADER_HOVERED"
            when: !root.clipSelected && headerDragArea.containsMouse
            PropertyChanges { target: header; color: ui.blendColors("#ffffff", root.clipColor, 0.8)}
            PropertyChanges { target: titleLabel; color: "#ffffff"}
            PropertyChanges { target: menuBtn; iconColor: "#ffffff"}
        },

        State {
            name: "SELECTED_HEADER_HOVERED"
            when: root.clipSelected && headerDragArea.containsMouse
            PropertyChanges { target: header; color: ui.blendColors("#ffffff", root.clipColor, 0.2) }
            PropertyChanges { target: titleLabel; color: "#000000"}
            PropertyChanges { target: menuBtn; iconColor: "#000000"}
        }
    ]
}
