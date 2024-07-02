import QtQuick

Rectangle {

    id: root

    width: 3
    color: "#ffffff"
    border.width: 1
    border.color: "#000000"

    Canvas {
        id: marker

        width: 17
        height: 18
        x: -(marker.width - 3)  / 2
        y: -marker.height

        property real radius: 2

        onPaint: {
            const ctx = getContext("2d")

            ctx.clearRect(0, 0, width, height)

            ctx.fillStyle = root.color
            ctx.strokeStyle = root.border.color
            ctx.lineWidth = 2

            ctx.beginPath()

            let b = ctx.lineWidth / 2

            ctx.moveTo(radius, b)

            ctx.lineTo(width - radius, b)
            ctx.quadraticCurveTo(width - b, b, width - b, radius)


            ctx.lineTo(width - b, (height / 2))
            // {
            //     let cx1 = width - radius - b
            //     let cx2 = height / 2 - b
            //     let ang1 = 45 * (Math.PI / 180)
            //     let x1 = cx1 + (radius * Math.cos(ang1));
            //     let y1 = cx2 + (radius * Math.sin(ang1));
            //     ctx.quadraticCurveTo(width - b, height / 2 - b, x1, y1)
            // }

            ctx.lineTo((width / 2) + radius, height - radius)
            ctx.quadraticCurveTo(width / 2, height, (width / 2) - radius, height - radius)


            ctx.lineTo(b, (height / 2))
           // ctx.quadraticCurveTo(width, height / 2, x1, y1)

            ctx.lineTo(b, radius)
            ctx.quadraticCurveTo(b, b, radius, b)

            ctx.stroke()
            ctx.fill()
        }
    }
}
