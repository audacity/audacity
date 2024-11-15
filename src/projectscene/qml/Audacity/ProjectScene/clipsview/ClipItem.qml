import QtQuick
import QtQuick.Layouts

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
    property int pitch: 0
    property int speedPercentage: 0.0
    property alias showChannelSplitter: channelSplitter.visible
    property alias channelHeightRatio: channelSplitter.channelHeightRatio
    property var canvas: null
    property color clipColor: "#677CE4"
    property bool clipSelected: false
    property bool isDataSelected: false
    property int selectionStart: 0
    property int selectionWidth: 0

    property real distanceToLeftNeighbor: -1
    property real distanceToRightNeighbor: -1

    property real leftVisibleMargin: 0
    property real rightVisibleMargin: 0

    property bool collapsed: false

    signal clipStartEditRequested()
    signal clipEndEditRequested()

    signal clipMoveRequested(bool completed)
    signal clipLeftTrimRequested(bool completed)
    signal clipRightTrimRequested(bool completed)

    signal requestSelected()
    signal ratioChanged(double val)

    signal pitchChangeRequested()
    signal pitchResetRequested()

    signal speedChangeRequested()
    signal speedResetRequested()

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

    readonly property string leftTrimShape: ":/images/customCursorShapes/ClipTrimLeft.png"
    readonly property string leftStretchShape: ":/images/customCursorShapes/ClipStretchLeft.png"
    readonly property string rightTrimShape: ":/images/customCursorShapes/ClipTrimRight.png"
    readonly property string rightStretchShape: ":/images/customCursorShapes/ClipStretchRight.png"

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

        hoverEnabled: true
        cursorShape: Qt.IBeamCursor
        acceptedButtons: Qt.RightButton

        onClicked: function(e) {
            contextMenuLoader.show(Qt.point(e.x, e.y), contextMenuModel.items)
            root.requestSelected()
        }

        onPositionChanged: {
            clipItemMousePositionChanged(mouseX, mouseY)
        }
    }

    CustomCursor {
        id: customCursor
        active: leftTrimStretchEdgeHover.containsMouse || rightTrimStretchEdgeHover.containsMouse
        source: leftTrimStretchEdgeHover.containsMouse ? leftTrimShape : rightTrimShape
    }

    MouseArea {
        id: leftTrimStretchEdgeHover

        x: distanceToLeftNeighbor >= -0.5 && distanceToLeftNeighbor <= 10 ? root.x - Math.min(distanceToLeftNeighbor / 2, 5) : root.x - 5
        width: distanceToLeftNeighbor >= -0.5 && distanceToLeftNeighbor <= 10 ? 6 + Math.min(distanceToLeftNeighbor / 2, 5) : 11
        z: headerDragArea.z + 1
        height: !root.collapsed ? root.height / 3 : root.height / 2

        anchors.top: root.top

        hoverEnabled: true
        visible: !root.clipSelected

        cursorShape: Qt.BlankCursor

        // make sure cursor is visible on top of nearby clips
        onContainsMouseChanged: {
            if (containsMouse) {
                root.parent.z = 1
            } else {
                root.parent.z = 0
            }
        }

        onPressed: function(e) {
            root.clipStartEditRequested()
        }

        onReleased: function(e) {
            if (e.modifiers & (Qt.AltModifier | Qt.MetaModifier)) {
                //! TODO AU4: stretch request
            } else {
                root.clipLeftTrimRequested(true)
            }

            // this needs to be always at the very end
            root.clipEndEditRequested()
        }

        onPositionChanged: function(e) {
            clipItemMousePositionChanged(e.x, e.y)

            if (e.modifiers & (Qt.AltModifier | Qt.MetaModifier)) {
                if (customCursor.source !== leftStretchShape) {
                    customCursor.source = leftStretchShape
                }
                //! TODO AU4: stretch request
            } else {
                if (customCursor.source !== leftTrimShape) {
                    customCursor.source = leftTrimShape
                }
                if (pressed) {
                    root.clipLeftTrimRequested(false)
                }
            }
        }
    }

    MouseArea {
        id: rightTrimStretchEdgeHover

        x: root.width - 5
        z: headerDragArea.z + 1
        width: distanceToRightNeighbor >= -0.5 && distanceToRightNeighbor <= 10 ? 6 + Math.min(distanceToRightNeighbor / 2, 5): 11
        height: !root.collapsed ? root.height / 3 : root.height / 2

        anchors.top: root.top

        hoverEnabled: true
        visible: !root.clipSelected

        cursorShape: Qt.BlankCursor

        // make sure cursor is visible on top of nearby clips
        onContainsMouseChanged: {
            if (containsMouse) {
                root.parent.z = 1
            } else {
                root.parent.z = 0
            }
        }

        onPressed: function(e) {
            root.clipStartEditRequested()
        }

        onReleased: function(e) {
            if (e.modifiers & (Qt.AltModifier | Qt.MetaModifier)) {
                //! TODO AU4: stretch request
            } else {
                root.clipRightTrimRequested(true)
            }

            // this needs to be always at the very end
            root.clipEndEditRequested()
        }

        onPositionChanged: function(e) {
            let mousePos = mapToItem(root, e.x, e.y)
            clipItemMousePositionChanged(mousePos.x, mousePos.y)

            if (e.modifiers & (Qt.AltModifier | Qt.MetaModifier)) {
                if (customCursor.source !== rightStretchShape) {
                    customCursor.source = rightStretchShape
                }
                //! TODO AU4: stretch request
            } else {
                if (customCursor.source !== rightTrimShape) {
                    customCursor.source = rightTrimShape
                }
                if (pressed) {
                    root.clipRightTrimRequested(false)
                }
            }
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

            Rectangle {
                id: headerSelectionRectangle

                x: root.selectionStart
                z: 0 // Ensure this is below the header content
                width: Math.min(root.selectionWidth, header.width)

                anchors.top: header.top
                anchors.bottom: header.bottom

                color: waveView.transformColor(clipColor)
                visible: root.isDataSelected
            }

            MouseArea {
                id: headerDragArea
                anchors.fill: parent

                hoverEnabled: true
                cursorShape: Qt.OpenHandCursor

                property bool moveActive: false

                onPositionChanged: function(e) {
                    root.clipItemMousePositionChanged(e.x, e.y)

                    if (pressed) {
                        root.clipMoveRequested(false)
                    }
                }

                onPressed: function(e) {
                    root.requestSelected()

                    root.clipStartEditRequested()
                }

                onReleased: function(e) {
                    root.clipMoveRequested(true)

                    root.clipEndEditRequested()
                }

                onDoubleClicked: root.editTitle()
            }

            StyledTextLabel {
                id: titleLabel
                anchors.top: parent.top
                anchors.bottom: parent.bottom
                anchors.left: parent.left
                anchors.right: buttonsRow.left
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

            Row {
                id: buttonsRow

                anchors.right: parent.right
                anchors.rightMargin: 4
                anchors.verticalCenter: parent.verticalCenter

                spacing: 2

                ClipItemPropertyButton {
                    id: pitchBtn

                    text: {
                        let semis = Math.trunc(root.pitch / 100)
                        let cents = Math.abs(root.pitch % 100).toString().padStart(2, "0")
                        return semis + (cents > "00" ? "." + cents : "")
                    }
                    icon: root.pitch > 0 ? IconCode.ARROW_UP : IconCode.ARROW_DOWN

                    visible: root.pitch !== 0

                    onClicked: function(mouse){
                        if (mouse.modifiers & Qt.ControlModifier) {
                            root.pitchResetRequested()
                        } else {
                            root.pitchChangeRequested()
                        }
                    }
                }

                ClipItemPropertyButton {
                    id: speedBtn

                    icon: IconCode.CLOCK
                    text: root.speedPercentage + "%"

                    visible: root.speedPercentage !== 100

                    onClicked: function(mouse){
                        if (mouse.modifiers & Qt.ControlModifier) {
                            root.speedResetRequested()
                        } else {
                            root.speedChangeRequested()
                        }
                    }
                }

                MenuButton {
                    id: menuBtn
                    width: 16
                    height: 16
                    anchors.verticalCenter: parent.verticalCenter

                    menuModel: contextMenuModel

                    onHandleMenuItem: function(itemId) {
                        Qt.callLater(contextMenuModel.handleMenuItem, itemId)
                    }

                    onClicked: {
                        root.requestSelected()
                    }
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

        onClipStartEditRequested: function() {
            root.clipStartEditRequested()
        }

        onClipEndEditRequested: function() {
            root.clipEndEditRequested()
        }

        onTrimLeftRequested: function(completed) {
            root.clipLeftTrimRequested(completed)
        }

        onTrimRightRequested: function(completed) {
            root.clipRightTrimRequested(completed)
        }
    }

    state: "NORMAL"
    states: [
        State {
            name: "NORMAL"
            when: !root.clipSelected && !headerDragArea.containsMouse
            PropertyChanges { target: header; color: root.clipColor }
            PropertyChanges { target: titleLabel; color: "#000000"}
            PropertyChanges { target: pitchBtn; textColor: "#000000"; iconColor: "#000000" }
            PropertyChanges { target: speedBtn; textColor: "#000000"; iconColor: "#000000" }
            PropertyChanges { target: menuBtn; iconColor: "#000000"}
        },

        State {
            name: "SELECTED"
            when: root.clipSelected && !headerDragArea.containsMouse
            PropertyChanges { target: header; color: ui.blendColors("#ffffff", root.clipColor, 0.3) }
            PropertyChanges { target: titleLabel; color: "#000000" }
            PropertyChanges { target: pitchBtn; textColor: "#000000"; iconColor: "#000000" }
            PropertyChanges { target: speedBtn; textColor: "#000000"; iconColor: "#000000" }
            PropertyChanges { target: menuBtn; iconColor: "#000000"}
        },

        State {
            name: "NORMAL_HEADER_HOVERED"
            when: !root.clipSelected && headerDragArea.containsMouse
            PropertyChanges { target: header; color: ui.blendColors("#ffffff", root.clipColor, 0.8)}
            PropertyChanges { target: titleLabel; color: "#ffffff"}
            PropertyChanges { target: pitchBtn; textColor: "#ffffff"; iconColor: "#ffffff" }
            PropertyChanges { target: speedBtn; textColor: "#ffffff"; iconColor: "#ffffff" }
            PropertyChanges { target: menuBtn; iconColor: "#ffffff"}
        },

        State {
            name: "SELECTED_HEADER_HOVERED"
            when: root.clipSelected && headerDragArea.containsMouse
            PropertyChanges { target: header; color: ui.blendColors("#ffffff", root.clipColor, 0.2) }
            PropertyChanges { target: titleLabel; color: "#000000"}
            PropertyChanges { target: pitchBtn; textColor: "#000000"; iconColor: "#000000" }
            PropertyChanges { target: speedBtn; textColor: "#000000"; iconColor: "#000000" }
            PropertyChanges { target: menuBtn; iconColor: "#000000"}
        }
    ]
}
