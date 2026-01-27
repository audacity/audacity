import QtQuick 2.15

import "internal"

Canvas {
    id: root

    property alias topLeft: topLeft
    property alias topRight: topRight
    property alias bottomRight: bottomRight
    property alias bottomLeft: bottomLeft

    readonly property var handleId: ({
            None: -1,
            TopLeft: 0,
            TopRight: 1,
            BottomLeft: 2,
            BottomRight: 3,
            Left: 4,
            Right: 5,
            Top: 6,
            Bottom: 7
        })

    function hitHandle(mouseX, mouseY, mouseParent) {
        if (prv.isOver(mouseX, mouseY, mouseParent, topLeft)) {
            return handleId.TopLeft
        } else if (prv.isOver(mouseX, mouseY, mouseParent, topRight)) {
            return handleId.TopRight
        } else if (prv.isOver(mouseX, mouseY, mouseParent, bottomRight)) {
            return handleId.BottomRight
        } else if (prv.isOver(mouseX, mouseY, mouseParent, bottomLeft)) {
            return handleId.BottomLeft
        } else if (prv.isOver(mouseX, mouseY, mouseParent, leftEdge)) {
            return handleId.Left
        } else if (prv.isOver(mouseX, mouseY, mouseParent, rightEdge)) {
            return handleId.Right
        } else if (prv.isOver(mouseX, mouseY, mouseParent, topEdge)) {
            return handleId.Top
        } else if (prv.isOver(mouseX, mouseY, mouseParent, bottomEdge)) {
            return handleId.Bottom
        } else {
            return handleId.None
        }
    }

    clip: false

    QtObject {
        id: prv

        function isOver(mouseX, mouseY, mouseParent, handle) {
            const pos = mouseParent.mapToItem(root, mouseX, mouseY)
            return pos.x >= handle.x && pos.x <= handle.x + handle.width && pos.y >= handle.y && pos.y <= handle.y + handle.height
        }

        readonly property int edgeWidth: 12
    }

    antialiasing: false
    onPaint: {
        var ctx = getContext("2d")
        ctx.clearRect(0, 0, width, height)
        ctx.lineWidth = 1;

        // Draw black dashes
        ctx.strokeStyle = "black"
        ctx.setLineDash([4, 4])
        ctx.lineDashOffset = 0
        ctx.strokeRect(0.5, 0.5, width - 1, height - 1);

        // Draw white dashes offset by 4px
        ctx.strokeStyle = "white"
        ctx.lineDashOffset = 4
        ctx.strokeRect(0.5, 0.5, width - 1, height - 1)
    }
    onWidthChanged: requestPaint()
    onHeightChanged: requestPaint()

    Item {
        id: leftEdge
        x: -leftEdge.width / 2
        y: topLeft.y + topLeft.height
        width: prv.edgeWidth
        height: parent.height
    }

    Item {
        id: rightEdge
        x: parent.width - rightEdge.width / 2
        y: topRight.y + topRight.height
        width: prv.edgeWidth
        height: parent.height
    }

    Item {
        id: topEdge
        x: topLeft.x + topLeft.width
        y: -topEdge.height / 2
        width: parent.width
        height: prv.edgeWidth
    }

    Item {
        id: bottomEdge
        x: bottomLeft.x + bottomLeft.width
        y: parent.height - bottomEdge.height / 2
        width: parent.width
        height: prv.edgeWidth
    }

    CornerHandle {
        id: topLeft
        x: -topLeft.width / 2
        y: -topLeft.height / 2
        size: prv.edgeWidth
    }

    CornerHandle {
        id: topRight
        x: parent.width - topRight.width / 2
        y: -topRight.height / 2
        size: prv.edgeWidth
    }

    CornerHandle {
        id: bottomRight
        x: parent.width - bottomRight.width / 2
        y: parent.height - bottomRight.height / 2
        size: prv.edgeWidth
    }

    CornerHandle {
        id: bottomLeft
        x: -bottomLeft.width / 2
        y: parent.height - bottomLeft.height / 2
        size: prv.edgeWidth
    }
}
