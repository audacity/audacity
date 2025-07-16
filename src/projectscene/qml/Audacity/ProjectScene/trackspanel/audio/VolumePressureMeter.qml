/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.UiComponents 1.0
import Muse.Ui 1.0

import Audacity.Playback 1.0

Canvas {
    id: root

    property real currentVolumePressure: -145.0
    property real currentRMS: -145.0
    property real minDisplayedVolumePressure: -60.0
    property real maxDisplayedVolumePressure: 0.0

    property var meterModel: null

    property real indicatorWidth
    property bool showClippedInfo: true

    property int recentPeakIntervalMiliseconds: 600

    property int overloadHeight: 4

    width: indicatorWidth

    onMeterModelChanged: {
        requestPaint()
    }

    QtObject {
        id: meterStyle

        readonly property var clippedColor: "#EF476F"
        readonly property var noClippedColor: ui.theme.buttonColor

        readonly property var rmsColor: ui.theme.accentColor
        readonly property var rmsOverlayColor: "#66000000"

        readonly property var defaultColor: ui.theme.accentColor

        readonly property var gradientColorGreen: "#50DF46"
        readonly property var gradientColorYellow: "#FFE100"
        readonly property var gradientColorRed: "#EF476F"

        readonly property color meterBackgroundColor: Utils.colorWithAlpha(ui.theme.strokeColor, 0.7)

        readonly property var maxPeakMarkerColor: "#14151A"

        readonly property var smallSteps: root.meterModel.smallSteps

        onSmallStepsChanged: {
            requestPaint()
        }

        function getRecentPeakMarkerColor() {
            switch (root.meterModel.meterStyle) {
                case PlaybackMeterStyle.Default:
                    return meterStyle.defaultColor
                case PlaybackMeterStyle.RMS:
                    return meterStyle.rmsColor
                case PlaybackMeterStyle.Gradient:
                    var recentPeakRatio = (prv.recentPeak - root.minDisplayedVolumePressure) / (root.maxDisplayedVolumePressure - root.minDisplayedVolumePressure)
                    if (recentPeakRatio < 0.2) {
                        return meterStyle.gradientColorGreen
                    } else if (recentPeakRatio < 0.8) {
                        return meterStyle.gradientColorYellow
                    }
                    return meterStyle.gradientColorRed
                default:
                    return meterStyle.maxPeakMarkerColor
            }
        }

        function createGradient(ctx, width, height) {
            const gradient = ctx.createLinearGradient(0, 0, width, height)
            gradient.addColorStop(0, gradientColorRed)
            gradient.addColorStop(0.2, gradientColorYellow)
            gradient.addColorStop(1.0, gradientColorGreen)

            return gradient
        }
    }

    QtObject {
        id: prv

        readonly property real indicatorHeight:  root.height - root.overloadHeight

        property bool needsClear: false

        property real updatedVolumePressure: -145.0
        property real maxPeak: -145.0
        property real recentPeak: -145.0
        property var recentVolumePressure: []

        property bool isClipping: updatedVolumePressure >= root.maxDisplayedVolumePressure
        property bool clipped: false

        function updateRecentPeak() {
            const now = Date.now()

            prv.recentVolumePressure.push({ value: prv.updatedVolumePressure, time: now })

            const recentPeakStartInterval = now - root.recentPeakIntervalMiliseconds
            let cutoffIndex = -1

            for (let i = 0; i < prv.recentVolumePressure.length; i++) {
                if (prv.recentVolumePressure[i].time < recentPeakStartInterval) {
                    cutoffIndex = i
                }
                else {
                    break
                }
            }

            if (cutoffIndex !== -1) {
                prv.recentVolumePressure.splice(0, cutoffIndex)
            }

            prv.recentPeak = Math.max(-145, ...prv.recentVolumePressure.map(item => item.value))
        }

        function sampleValueToHeight(sampleValue) {
            if (!root.meterModel) {
                return;
            }

            return prv.indicatorHeight * root.meterModel.sampleToPosition(sampleValue); 
        }

        function drawRoundedRect(ctx, fillStyle, x, y, width, height, radius, roundedEdge) {
            ctx.save();
            ctx.fillStyle = fillStyle;
            ctx.beginPath();

            if (roundedEdge === "top" || roundedEdge === "both") {
                ctx.moveTo(x + radius, y);
                ctx.arcTo(x + width, y, x + width, y + radius, radius);
            } else {
                ctx.moveTo(x, y);
                ctx.lineTo(x + width, y);
            }

            if (roundedEdge === "bottom" || roundedEdge === "both") {
                ctx.lineTo(x + width, y + height - radius);
                ctx.arcTo(x + width, y + height, x + width - radius, y + height, radius);
                ctx.lineTo(x + radius, y + height);
                ctx.arcTo(x, y + height, x, y + height - radius, radius);
            } else {
                ctx.lineTo(x + width, y + height);
                ctx.lineTo(x, y + height);
            }

            if (roundedEdge === "top" || roundedEdge === "both") {
                ctx.lineTo(x, y + radius);
                ctx.arcTo(x, y, x + radius, y, radius);
            } else {
                ctx.lineTo(x, y);
            }

            ctx.closePath();
            ctx.fill();
            ctx.restore();
        }
        
        onIsClippingChanged: {
            if (prv.isClipping) {
                prv.clipped = true
                root.requestPaint()
            }
        }
    }

    function reset() {
        prv.maxPeak = -145
        prv.recentPeak = -145
        prv.updatedVolumePressure = -145
        prv.recentVolumePressure = []

        requestPaint()
    }

    function resetClipped() {
        prv.clipped = false
        requestPaint()
    }

    function drawBackground(ctx) {
        ctx.clearRect(0, 0, root.indicatorWidth, root.height)

        const fillStyle = meterStyle.meterBackgroundColor
        prv.drawRoundedRect(ctx, fillStyle, 0, 0, root.indicatorWidth, root.height, 2, "both")
    }

    function drawClippedIndicator(ctx) {
        const fillStyle = prv.clipped ? meterStyle.clippedColor : meterStyle.noClippedColor
        prv.drawRoundedRect(ctx, fillStyle, 0, 0, root.indicatorWidth, root.overloadHeight, 2, "top")
    }

    function drawMeterBar(ctx) {
        if (root.meterModel.meterStyle == PlaybackMeterStyle.Default) {
            drawBarStyleDefault(ctx)
        } else if (root.meterModel.meterStyle == PlaybackMeterStyle.RMS) {
            drawBarStyleRMS(ctx)
        } else if (root.meterModel.meterStyle == PlaybackMeterStyle.Gradient) {
            drawBarStyleGradient(ctx)
        }
    }

    function drawPeakMarkers(ctx) {
        const recentPeakHeight = prv.sampleValueToHeight(prv.recentPeak)
        if (recentPeakHeight > 0) {
            ctx.fillStyle = meterStyle.getRecentPeakMarkerColor()
            ctx.fillRect(0, root.height - recentPeakHeight, root.indicatorWidth, 1)
        }

        const maxPeakHeight = prv.sampleValueToHeight(prv.maxPeak)
        if (maxPeakHeight > 0) {
            ctx.fillStyle = meterStyle.maxPeakMarkerColor
            ctx.fillRect(0, root.height - maxPeakHeight, root.indicatorWidth, 1)
        }
    }

    function drawBarStyleDefault(ctx) {
        // On clipping draw full red rectangle
        if (prv.isClipping) {
            ctx.fillStyle = meterStyle.clippedColor
            ctx.fillRect(0, 0, root.indicatorWidth, prv.indicatorHeight + root.overloadHeight)
            return
        }

        // Draw the volume pressure
        const meterHeight = prv.sampleValueToHeight(prv.updatedVolumePressure)
        if (meterHeight > 0) {
            prv.drawRoundedRect(ctx, meterStyle.defaultColor, 0, root.height - meterHeight, indicatorWidth, meterHeight, 2, "bottom")
        }

        drawPeakMarkers(ctx)
    }

    function drawBarStyleRMS(ctx) {
        // On clipping draw full red rectangle
        if (prv.isClipping) {
            ctx.fillStyle = meterStyle.clippedColor
            ctx.fillRect(0, 0, indicatorWidth, prv.indicatorHeight + root.overloadHeight)
            return
        }

        var yRMS = prv.sampleValueToHeight(root.currentRMS)
        var yPeak = prv.sampleValueToHeight(root.currentVolumePressure)

        if (yRMS > 0) {
            prv.drawRoundedRect(ctx, meterStyle.rmsColor, 0, root.height - yPeak, root.indicatorWidth, yPeak, 2, "bottom")

            ctx.fillStyle = meterStyle.rmsOverlayColor
            ctx.fillRect(0, root.height - yPeak, root.indicatorWidth, yPeak - yRMS)
        }

        drawPeakMarkers(ctx)
    }

    function drawBarStyleGradient(ctx) {
        // Draw the volume pressure
        const meterHeight = prv.sampleValueToHeight(prv.updatedVolumePressure)
        if (meterHeight > 0) {
            const fillStyle = meterStyle.createGradient(ctx, 0, prv.indicatorHeight)
            prv.drawRoundedRect(ctx, fillStyle, 0, root.height - meterHeight, indicatorWidth, meterHeight, 2, "bottom")
        }

        drawPeakMarkers(ctx)
    }

    onPaint: {
        var ctx = root.context

        if (!ctx) {
            ctx = getContext("2d")

            if (ui.currentLanguageLayoutDirection() === Qt.RightToLeft) {
                ctx.textAlign = "end"
            } else {
                ctx.textAlign = "start"
            }
        }

        drawBackground(ctx)

        if (root.showClippedInfo) {
            drawClippedIndicator(ctx)
        }

        if (prv.needsClear) {
            // Just don´t draw anything else
            prv.needsClear = false
            return
        }
        drawMeterBar(ctx)
    }

    onHeightChanged: {
        requestPaint();
    }

    onCurrentVolumePressureChanged: {
        if (isNaN(root.currentVolumePressure)) {
            return
        }

        prv.maxPeak = Math.max(prv.maxPeak, root.currentVolumePressure)
        prv.updatedVolumePressure = root.currentVolumePressure
        prv.updateRecentPeak()
        requestPaint()
    }

    Component.onCompleted: {
        requestPaint()
    }
}
