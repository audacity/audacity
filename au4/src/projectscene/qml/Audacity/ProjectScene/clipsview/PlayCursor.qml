import QtQuick

Rectangle {

    id: root

    property color borderColor: "#000000"
    property int borderWidth: 1
    property bool timelinePressed: false

    Rectangle {
        id: cursor

        x: -width / 2
        width: 3
        height: root.height
        color: "#ffffff"


        // draw borders without top one
        Rectangle {
            id: leftBorder
            color: borderColor
            width: borderWidth
            height: parent.height
            anchors.left: parent.left
        }
        Rectangle {
            id: rightBorder
            color: borderColor
            width: borderWidth
            height: parent.height
            anchors.right: parent.right
        }
        Rectangle {
            id: bottomBorder
            color: borderColor
            width: root.width
            height: borderWidth
            anchors.bottom: parent.bottom
        }
    }

    signal setPlaybackPosition(real x)
    signal playCursorMousePositionChanged(real x)

    Canvas {
        id: marker

        width: 17
        height: baseRectHeight + vectorPulldown

        property real baseRectHeight: 11
        property real vectorPulldown: 5

        x: -(marker.width) / 2
        y: -marker.height

        property real radius: 2

        MouseArea {
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: pressed || timelinePressed ? Qt.ClosedHandCursor : Qt.OpenHandCursor

            onPositionChanged: function(e) {
                var ix = mapToItem(root.parent, e.x, e.y).x
                if (pressed) {
                    setPlaybackPosition(ix)
                }
                playCursorMousePositionChanged(ix)
            }
        }

        onPaint: {
            const ctx = getContext("2d")

            ctx.clearRect(0, 0, width, height)

            ctx.fillStyle = root.color
            ctx.strokeStyle = root.border.color
            ctx.lineWidth = 2

            ctx.beginPath()

            let b = ctx.lineWidth / 2

            // start with top-left corner, move clockwise
            ctx.moveTo(radius, b)

            ctx.lineTo(width - radius, b)
            ctx.quadraticCurveTo(width - b, b + 1, width - b, radius)

            ctx.lineTo(width - b, (baseRectHeight - 1))
            {
                let cx1 = width - radius - b
                let cx2 = baseRectHeight - b
                let ang1 = 45 * (Math.PI / 180)
                let x1 = cx1 + (radius * Math.cos(ang1));
                let y1 = cx2 + (radius * Math.sin(ang1));
                ctx.quadraticCurveTo(width - b, baseRectHeight - b, x1, y1)
            }

            ctx.lineTo((width / 2) + radius, height - radius)
            ctx.quadraticCurveTo(width / 2, height, (width / 2) - radius, height - radius)

            {
                let cx1 = b
                let cx2 = baseRectHeight - b
                let ang1 = 135 * (Math.PI / 180)
                let x1 = cx1 + (radius * Math.cos(ang1));
                let y1 = cx2 + (radius * Math.sin(ang1));
                ctx.lineTo(b - x1, y1)
            }

            ctx.quadraticCurveTo(b, baseRectHeight - b, b, (baseRectHeight - 1))

            ctx.lineTo(b, radius)
            ctx.quadraticCurveTo(b, b, radius, b)

            ctx.stroke()
            ctx.fill()
        }
    }
}
