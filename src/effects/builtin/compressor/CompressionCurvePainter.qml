import QtQuick 2.15

Canvas {
    required property var model
    required property int min
    required property int max

    onPaint: {
        let ctx = getContext("2d")
        ctx.clearRect(0, 0, width, height)

        const curve = model.compressionCurve(min, max, 40)
        const points = curve.map(pt => ({
                    x: (pt.x - min) / (max - min) * width,
                    y: height - (pt.y - min) / (max - min) * height
                }));

        // Draw area under curve
        ctx.beginPath()
        ctx.moveTo(points[0].x, height)
        // Start at bottom left
        points.forEach(pt => ctx.lineTo(pt.x, pt.y))
        ctx.lineTo(points[points.length - 1].x, height)
        // Down to bottom right
        ctx.closePath()
        ctx.fillStyle = "#1A50CAFF"
        ctx.fill();

        // Draw curve
        ctx.strokeStyle = "#50caff"
        ctx.lineWidth = 1
        ctx.beginPath()
        ctx.moveTo(points[0].x, points[0].y)
        points.slice(1).forEach(pt => ctx.lineTo(pt.x, pt.y))
        ctx.stroke()
    }
}
