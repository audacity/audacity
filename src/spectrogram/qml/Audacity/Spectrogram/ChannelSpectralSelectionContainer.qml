import QtQuick
import Audacity.Spectrogram
import Audacity.UiComponents

Item {
    id: root

    required property var canvas
    required property int trackId
    required property int channel
    required property real trackSampleRate
    required property bool selectionInProgress
    required property real selectionStartPosition
    required property real selectionEndPosition
    required property real selectionStartFrequency
    required property real selectionEndFrequency
    required property real selectionStartTime
    required property real selectionEndTime
    required property var selectionController

    property alias verticalDragActive: selectionModel.verticalDragActive

    property real pointerFrequency: -1

    signal selectionHorizontalResize(real startPosition, real endPosition, bool completed)
    signal mousePositionChanged(real x, real y)

    QtObject {
        id: prv

        readonly property int cornerSize: 6
        readonly property int edgeWidth: 10
        property var init: ({})
        property bool dragging: Object.keys(init).length !== 0
        property bool selectionIsEmpty: root.selectionStartFrequency === -1 || root.selectionEndFrequency === -1 || (root.selectionStartFrequency === root.selectionEndFrequency && root.selectionStartTime === root.selectionEndTime)

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
        channel: root.channel
        channelHeight: root.height
        selectionStartFrequency: root.selectionStartFrequency
        selectionEndFrequency: root.selectionEndFrequency
        selectionStartTime: root.selectionStartTime
        selectionEndTime: root.selectionEndTime

        onCenterFrequencyChangeRequested: function (frequency) {
            selectionController.dragFrequencySelectionCenterFrequency(frequency)
        }
    }

    MouseArea {
        anchors.fill: parent

        visible: !selectionModel.verticalDragActive && (pressed || !prv.selectionIsEmpty)

        hoverEnabled: false
        acceptedButtons: Qt.LeftButton

        property var draggedEdges: []

        onPressed: function (mouse) {
            switch (marquee.hitHandle(mouse.x - marquee.x, mouse.y - marquee.y, marquee)) {
            case marquee.handleId.TopLeft:
                draggedEdges = [topEdge, leftEdge]
                root.pointerFrequency = Qt.binding(function () {
                    return root.selectionStartFrequency
                })
                break
            case marquee.handleId.TopRight:
                draggedEdges = [topEdge, rightEdge]
                root.pointerFrequency = Qt.binding(function () {
                    return root.selectionStartFrequency
                })
                break
            case marquee.handleId.BottomRight:
                draggedEdges = [bottomEdge, rightEdge]
                root.pointerFrequency = Qt.binding(function () {
                    return root.selectionEndFrequency
                })
                break
            case marquee.handleId.BottomLeft:
                draggedEdges = [bottomEdge, leftEdge]
                root.pointerFrequency = Qt.binding(function () {
                    return root.selectionEndFrequency
                })
                break
            case marquee.handleId.Left:
                draggedEdges = [leftEdge]
                root.pointerFrequency = -1
                break
            case marquee.handleId.Right:
                draggedEdges = [rightEdge]
                root.pointerFrequency = -1
                break
            case marquee.handleId.Top:
                draggedEdges = [topEdge]
                root.pointerFrequency = -1
                break
            case marquee.handleId.Bottom:
                draggedEdges = [bottomEdge]
                root.pointerFrequency = -1
                break
            default:
                draggedEdges = []
                root.pointerFrequency = -1
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
            }
            root.mousePositionChanged(mouse.x, mouse.y)
        }

        onReleased: function (mouse) {
            for (let i = 0; i < draggedEdges.length; i++) {
                prv.edgeReleased(draggedEdges[i], mouse)
            }
            draggedEdges = []
        }
    }

    MarqueeSelection {
        id: marquee

        visible: !prv.selectionIsEmpty

        x: root.selectionStartPosition
        y: selectionModel.selectionY
        width: root.selectionEndPosition - root.selectionStartPosition
        height: selectionModel.selectionHeight

        lineWidth: 1
        color: "white"
        changeCursorShape: !root.selectionInProgress

        Item {
            id: centerFrequencyDragHandle

            anchors.left: parent.left
            anchors.right: parent.right
            anchors.verticalCenter: parent.verticalCenter

            height: 10

            MouseArea {
                id: centerFrequencyMouseArea

                anchors.fill: parent
                cursorShape: Qt.SizeVerCursor

                drag.target: parent
                drag.axis: Drag.YAxis

                onPressed: {
                    selectionModel.startCenterFrequencyDrag()
                    root.pointerFrequency = Qt.binding(function () {
                        return selectionModel.centerFrequency
                    })
                }

                onPositionChanged: function (mouse) {
                    const position = mapToItem(root, mouse.x, mouse.y)
                    selectionModel.dragCenterFrequency(position.y)
                    root.mousePositionChanged(position.x, mapToItem(root, 0, height / 2).y)
                }

                onReleased: {
                    selectionModel.endCenterFrequencyDrag()
                }

                onCanceled: {
                    selectionModel.endCenterFrequencyDrag()
                }
            }

            Canvas {
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.verticalCenter: parent.verticalCenter
                height: marquee.lineWidth

                antialiasing: false

                onPaint: {
                    var ctx = getContext("2d")
                    ctx.clearRect(0, 0, width, height)
                    ctx.lineWidth = marquee.lineWidth

                    ctx.beginPath()
                    ctx.moveTo(0.5, height / 2)
                    ctx.lineTo(width - 1, height / 2)

                    ctx.setLineDash([4, 4])
                    ctx.strokeStyle = "black"
                    ctx.lineDashOffset = 0
                    ctx.stroke()
                    ctx.strokeStyle = marquee.color
                    ctx.lineDashOffset = 4
                    ctx.stroke()
                }
            }
        }
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
