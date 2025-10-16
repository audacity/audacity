import QtQuick

Canvas {
    id: root

    property bool isRight: false
    property bool enableCursorInteraction: true
    property color backgroundColor: "transparent"

    property alias hovered: dragArea.containsMouse

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
        cursorShape: Qt.SizeHorCursor

        visible: root.enableCursorInteraction
    }
}
