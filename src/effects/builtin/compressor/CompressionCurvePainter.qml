import QtQuick 2.15

Canvas {
    required property var model

    onPaint: {
        let ctx = getContext("2d");
        ctx.clearRect(0, 0, width, height);

        const dbIn = [];
        const x = [];
        const n = 40;
        for (let i = 0; i <= n; ++i) {
            const db = prv.min + (prv.max - prv.min) * i / n;
            x.push(width * (db - prv.min) / (prv.max - prv.min));
            dbIn.push(db);
        }

        const dbOut = model.compressionCurve(dbIn);
        const points = [];
        for (let i = 0; i < dbIn.length; ++i) {
            const db = dbIn[i];
            const yValue = dbOut[i];
            // Map yValue to canvas coordinates (assuming yValue in [prv.min, prv.max])
            const y = height * (1 - (yValue - prv.min) / (prv.max - prv.min));
            points.push({x: x[i], y: y});
        }

        // Draw area under curve
        ctx.beginPath();
        ctx.moveTo(points[0].x, height); // Start at bottom left
        for (let j = 0; j < points.length; ++j) {
            const pt = points[j];
            ctx.lineTo(pt.x, pt.y);
        }
        ctx.lineTo(points[points.length - 1].x, height); // Down to bottom right
        ctx.closePath();
        ctx.fillStyle = "rgba(80,202,255,0.1)";
        ctx.fill();

        // Draw curve
        ctx.strokeStyle = "#50caff";
        ctx.lineWidth = 1;
        ctx.beginPath();
        for (let j = 0; j < points.length; ++j) {
            const pt = points[j];
            if (j === 0)
                ctx.moveTo(pt.x, pt.y);
            else
                ctx.lineTo(pt.x, pt.y);
        }
        ctx.stroke();
    }
}
