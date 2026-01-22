import QtQuick
import Audacity.Spectrogram
import Audacity.UiComponents

Item {
    id: root

    required property var canvas
    required property int trackId
    required property int channel
    required property real trackSampleRate
    required property real selectionStartPosition
    required property real selectionEndPosition
    required property real selectionStartFrequency
    required property real selectionEndFrequency
    required property var selectionController

    signal selectionHorizontalResize(real startPosition, real endPosition, bool completed)

    QtObject {
        id: prv

        readonly property int cornerSize: 6
        readonly property int edgeWidth: 10
        property var init: ({})
        property bool dragging: Object.keys(init).length !== 0
        property bool selectionIsEmpty: root.selectionStartFrequency === root.selectionEndFrequency

        function isDragged(name) {
            return dragging && name in init
        }

        function startDrag(name, data) {
            let newInit = Object.assign({}, init)
            newInit[name] = data
            init = newInit
        }

        function endDrag(name) {
            let newInit = Object.assign({}, init)
            delete newInit[name]
            init = newInit
        }

        function clamp(value, min, max) {
            return Math.min(Math.max(value, min), max)
        }

        function getGlobalY() {
            return root.mapToItem(root.canvas, 0, 0).y
        }

        // Common edge handler functions
        function edgePressed(edge, mouse) {
            startDrag(edge.name, edge.initData(mouse))
            if (edge.name !== "left" && edge.name !== "right") {
                const hit = SpectrogramHitFactory.createSpectrogramHit(root.trackId, root.channel, prv.getGlobalY(), root.height)
                root.selectionController.startSelectionVerticalResize(hit)
            }
        }

        function edgePositionChanged(edge, mouse) {
            if (isDragged(edge.name)) {
                edge.updatePosition(mouse, false)
            }
        }

        function edgeReleased(edge, mouse) {
            if (isDragged(edge.name)) {
                edge.updatePosition(mouse, true)
                endDrag(edge.name)
            }
        }
    }

    ChannelSpectralSelectionModel {
        id: selectionModel

        trackId: root.trackId
        trackSampleRate: root.trackSampleRate
        channelHeight: root.height
        selectionStartFrequency: root.selectionStartFrequency
        selectionEndFrequency: root.selectionEndFrequency
    }

    MouseArea {
        anchors.fill: parent

        visible: pressed || !prv.selectionIsEmpty

        hoverEnabled: true
        acceptedButtons: Qt.LeftButton

        cursorShape: Qt.CrossCursor

        property var draggedEdges: []

        onPressed: function (mouse) {
            switch (marquee.hitHandle(mouse.x, mouse.y, root)) {
            case marquee.handleId.TopLeft:
                draggedEdges = [topEdge, leftEdge]
                break
            case marquee.handleId.TopRight:
                draggedEdges = [topEdge, rightEdge]
                break
            case marquee.handleId.BottomRight:
                draggedEdges = [bottomEdge, rightEdge]
                break
            case marquee.handleId.BottomLeft:
                draggedEdges = [bottomEdge, leftEdge]
                break
            case marquee.handleId.Left:
                draggedEdges = [leftEdge]
                break
            case marquee.handleId.Right:
                draggedEdges = [rightEdge]
                break
            case marquee.handleId.Top:
                draggedEdges = [topEdge]
                break
            case marquee.handleId.Bottom:
                draggedEdges = [bottomEdge]
                break
            default:
                draggedEdges = []
                // It missed - let the event propagate
                mouse.accepted = false
                return
            }

            for (let i = 0; i < draggedEdges.length; i++) {
                prv.edgePressed(draggedEdges[i], mouse)
            }
        }

        onPositionChanged: function (mouse) {
            if (pressed) {
                for (let i = 0; i < draggedEdges.length; i++) {
                    prv.edgePositionChanged(draggedEdges[i], mouse)
                }
            } else {
                switch (marquee.hitHandle(mouse.x, mouse.y, root)) {
                case marquee.handleId.TopLeft:
                case marquee.handleId.BottomRight:
                    cursorShape = Qt.SizeFDiagCursor
                    break
                case marquee.handleId.TopRight:
                case marquee.handleId.BottomLeft:
                    cursorShape = Qt.SizeBDiagCursor
                    break
                case marquee.handleId.Left:
                case marquee.handleId.Right:
                    cursorShape = Qt.SizeHorCursor
                    break
                case marquee.handleId.Top:
                case marquee.handleId.Bottom:
                    cursorShape = Qt.SizeVerCursor
                    break
                default:
                    cursorShape = Qt.CrossCursor
                    break
                }
            }
        }

        onReleased: function (mouse) {
            for (let i = 0; i < draggedEdges.length; i++) {
                prv.edgeReleased(draggedEdges[i], mouse)
            }
            draggedEdges = []
            cursorShape = Qt.CrossCursor
        }
    }

    MarqueeSelection {
        id: marquee

        visible: !prv.selectionIsEmpty

        x: root.selectionStartPosition
        y: selectionModel.selectionY
        width: root.selectionEndPosition - root.selectionStartPosition
        height: selectionModel.selectionHeight
    }

    QtObject {
        id: leftEdge
        readonly property string name: "left"

        function initData(mouse) {
            return {
                mouseX: mouse.x,
                leftPos: root.selectionStartPosition,
                rightPos: root.selectionEndPosition
            }
        }

        function updatePosition(mouse, complete) {
            const init = prv.init[name]
            const delta = mouse.x - init.mouseX
            const newPosition = init.leftPos + delta
            const x1 = Math.min(newPosition, init.rightPos)
            const x2 = Math.max(newPosition, init.rightPos)
            root.selectionHorizontalResize(x1, x2, complete)
        }
    }

    QtObject {
        id: rightEdge
        readonly property string name: "right"

        function initData(mouse) {
            return {
                mouseX: mouse.x,
                leftPos: root.selectionStartPosition,
                rightPos: root.selectionEndPosition
            }
        }

        function updatePosition(mouse, complete) {
            const init = prv.init[name]
            const delta = mouse.x - init.mouseX
            const newPosition = init.rightPos + delta
            const x1 = Math.min(init.leftPos, newPosition)
            const x2 = Math.max(init.leftPos, newPosition)
            root.selectionHorizontalResize(x1, x2, complete)
        }
    }

    QtObject {
        id: topEdge
        readonly property string name: "top"

        function initData(mouse) {
            return {
                mouseY: mouse.y,
                topPos: prv.getGlobalY() + selectionModel.selectionY,
                bottomPos: prv.getGlobalY() + selectionModel.selectionY + selectionModel.selectionHeight
            }
        }

        function updatePosition(mouse, complete) {
            const init = prv.init[name]
            const delta = mouse.y - init.mouseY
            const newTopPos = init.topPos + delta
            root.selectionController.updateSelectionVerticalResize(newTopPos, init.bottomPos, complete)
        }
    }

    QtObject {
        id: bottomEdge
        readonly property string name: "bottom"

        function initData(mouse) {
            return {
                mouseY: mouse.y,
                topPos: prv.getGlobalY() + selectionModel.selectionY,
                bottomPos: prv.getGlobalY() + selectionModel.selectionY + selectionModel.selectionHeight
            }
        }

        function updatePosition(mouse, complete) {
            const init = prv.init[name]
            const delta = mouse.y - init.mouseY
            const newBottomPos = init.bottomPos + delta
            root.selectionController.updateSelectionVerticalResize(init.topPos, newBottomPos, complete)
        }
    }
}
