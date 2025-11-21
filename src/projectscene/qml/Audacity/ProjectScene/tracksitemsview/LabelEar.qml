import QtQuick

Canvas {
    id: root

    property bool isRight: false
    property bool enableCursorInteraction: true
    property color backgroundColor: "transparent"
    property bool isSelected: false

    property bool isLinked: false

    property alias hovered: dragArea.containsMouse

    signal stretchRequested(bool completed)
    signal stretchStartRequested()
    signal stretchEndRequested()
    signal stretchMousePositionChanged(real x, real y)

    width: 7

    onPaint: {
        var ctx = getContext("2d")
        ctx.clearRect(0, 0, width, height)

        ctx.fillStyle = root.backgroundColor
        ctx.strokeStyle = root.backgroundColor
        ctx.lineWidth = 1
        ctx.imageSmoothingEnabled = true

        ctx.beginPath()

        if (root.isRight) {
            ctx.moveTo(0, 0)
            ctx.lineTo(0, height)
            ctx.lineTo(width, 0)
        } else {
            ctx.moveTo(0, 0)
            ctx.lineTo(width, 0)
            ctx.lineTo(width, height)
        }

        ctx.closePath()

        ctx.fill()
        ctx.stroke()
    }

    onBackgroundColorChanged: {
        root.requestPaint()
    }

    MouseArea {
        id: dragArea
        anchors.fill: parent

        acceptedButtons: Qt.LeftButton
        hoverEnabled: true
        cursorShape: !root.isLinked ? Qt.SizeHorCursor : Qt.SplitHCursor

        visible: root.enableCursorInteraction

        onPressed: function(e) {
            let mousePos = mapToItem(root.parent, e.x, e.y)
            root.stretchMousePositionChanged(mousePos.x, mousePos.y)
            root.stretchStartRequested()
            root.stretchRequested(false)
            e.accepted = true
        }

        onPositionChanged: function(e) {
            if (pressed) {
                let mousePos = mapToItem(root.parent, e.x, e.y)
                root.stretchMousePositionChanged(mousePos.x, mousePos.y)
                root.stretchRequested(false)
                e.accepted = true
            } else {
                e.accepted = false
            }
        }

        onReleased: function(e) {
            root.stretchRequested(true)
            root.stretchEndRequested()
            e.accepted = true
        }
    }
}
