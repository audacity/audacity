/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.UiComponents 1.0
import Muse.Ui 1.0

import Audacity.Playback 1.0

Canvas {
    id: root

    property real currentVolumePressure: -60.0
    property real currentRMS: -60.0
    property real minDisplayedVolumePressure: -60.0
    property real maxDisplayedVolumePressure: 0.0
    property bool isPlaying: false
    property bool isRecording: false

    property int meterStyle: PlaybackMeterStyle.Default

    property real indicatorWidth
    property bool showRuler: false

    property int recentPeakIntervalMiliseconds: 600

    width: root.showRuler ? indicatorWidth + 20 : indicatorWidth

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

        function getRecentPeakMarkerColor() {
            switch (root.meterStyle) {
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

        readonly property int overloadHeight: 4

        readonly property real indicatorHeight: root.height - prv.overloadHeight - 6

        // value ranges
        readonly property int fullValueRangeLength: root.maxDisplayedVolumePressure - root.minDisplayedVolumePressure
        readonly property real heightPerUnit: (prv.indicatorHeight - prv.overloadHeight) / fullValueRangeLength

        property bool needsClear: false

        property real updatedVolumePressure: -60.0
        property real maxPeak: -60.0
        property real recentPeak: -60.0
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

            prv.recentPeak = Math.max(-60, ...prv.recentVolumePressure.map(item => item.value))
        }

        function sampleValueToHeight(sampleValue) {
            const clampedValue = Math.max(root.minDisplayedVolumePressure,
                                    Math.min(sampleValue, root.maxDisplayedVolumePressure));

            return prv.heightPerUnit * (clampedValue - root.minDisplayedVolumePressure)

        }
        
        onIsClippingChanged: {
            if (prv.isClipping) {
                prv.clipped = true
                root.requestPaint()
            }
        }
    }

    QtObject {
        id: ruler

        readonly property real strokeHorizontalMargin: 2
        readonly property real longStrokeHeight: 1
        readonly property real longStrokeWidth: 5
        readonly property color longStrokeColor: Utils.colorWithAlpha(ui.theme.fontPrimaryColor, 0.5)
        readonly property real shortStrokeHeight: 1
        readonly property real shortStrokeWidth: 2
        readonly property color shortStrokeColor: Utils.colorWithAlpha(ui.theme.fontPrimaryColor, 0.3)

        // Rounding up fullStep value to the predefined one,
        // to avoid getting funny intervals like 3, or 7
        function roundUpToFixedValue(value) {
            // full and small step is
            // a number units per each respective notch on the ruler
            const steps = [
                { fullStep: 1, smallStep: 0 },
                { fullStep: 2, smallStep: 1 },
                { fullStep: 6, smallStep: 2 },
                { fullStep: 10, smallStep: 2 },
                { fullStep: 20, smallStep: 4 },
                { fullStep: 50, smallStep: 10 },
                { fullStep: 100, smallStep: 20 }
            ];

            // Find the nearest full step
            for (let i = 0; i < steps.length; i++) {
                if (value <= steps[i].fullStep) {
                    return steps[i];
                }
            }

            // Should not happen
            return steps[steps.length - 1];
        }

        function drawRuler(ctx) {
            var originVPos = prv.overloadHeight
            var originHPos = indicatorWidth + ruler.strokeHorizontalMargin

            ctx.clearRect(indicatorWidth, 0, root.width - indicatorWidth, root.height)
            ctx.font = prv.unitTextFont

            // Minimal height of a single full step
            const minimalFullStepHeight = 20;
            // Number of full steps to draw
            const fullStepCount = Math.ceil(root.height / minimalFullStepHeight);
            // Number of units per full step
            const unitsPerStep = root.maxDisplayedVolumePressure - root.minDisplayedVolumePressure;
            // Calculating normalized full and small step value
            const { fullStep, smallStep } = roundUpToFixedValue(unitsPerStep / fullStepCount);
            // Number of small steps to draw
            const smallStepCount = smallStep ? unitsPerStep / smallStep : 0

            // Drawing small steps
            for (let k = 1; k < smallStepCount; k++) {
                const vPos = originVPos + prv.heightPerUnit * smallStep * k;
                ctx.fillStyle = ruler.shortStrokeColor
                ctx.fillRect(originHPos, vPos, ruler.shortStrokeWidth, ruler.shortStrokeHeight)
            }

            // Drawing full steps
            for (let j = 0; j <= fullStepCount; j++) {
                const vPos = originVPos + prv.heightPerUnit * fullStep * j;

                // We don´t draw the first stroke
                if (j == 0) {
                    let textHPos = originHPos + ruler.strokeHorizontalMargin
                    ctx.fillStyle = prv.unitTextColor
                    ctx.fillText(fullStep * j, textHPos, vPos + 4)
                } else {
                    ctx.fillStyle = ruler.longStrokeColor
                    ctx.fillRect(originHPos, vPos, ruler.longStrokeWidth, ruler.longStrokeHeight)

                    let textHPos = originHPos + ruler.longStrokeWidth + ruler.strokeHorizontalMargin
                    ctx.fillStyle = prv.unitTextColor
                    ctx.fillText(fullStep * j, textHPos, vPos + 4)
                }
            }
        }
    }

    function reset() {
        prv.maxPeak = -60
        prv.recentPeak = -60
        prv.recentVolumePressure = []
        prv.updatedVolumePressure = -60

        prv.clipped = false

        requestPaint()
    }

    function drawBackground(ctx) {
        ctx.clearRect(0, 0, root.indicatorWidth, prv.indicatorHeight)

        ctx.fillStyle = meterStyle.meterBackgroundColor
        ctx.fillRect(0, 0, root.indicatorWidth, prv.indicatorHeight)
    }

    function drawClippedIndicator(ctx) {
        ctx.fillStyle = prv.clipped ? meterStyle.clippedColor : meterStyle.noClippedColor
        ctx.fillRect(0, 0, root.indicatorWidth, prv.overloadHeight)
    }

    function drawMeterBar(ctx) {
        if (root.meterStyle == PlaybackMeterStyle.Default) {
            drawBarStyleDefault(ctx)
        } else if (root.meterStyle == PlaybackMeterStyle.RMS) {
            drawBarStyleRMS(ctx)
        } else if (root.meterStyle == PlaybackMeterStyle.Gradient) {
            drawBarStyleGradient(ctx)
        }
    }

    function drawPeakMarkers(ctx) {
        const recentPeakHeight = prv.sampleValueToHeight(prv.recentPeak)
        if (recentPeakHeight > 0) {
            ctx.fillStyle = meterStyle.getRecentPeakMarkerColor()
            ctx.fillRect(0, root.height - 10 - recentPeakHeight, root.indicatorWidth, 1)
        }

        const maxPeakHeight = prv.sampleValueToHeight(prv.maxPeak)
        if (maxPeakHeight > 0) {
            ctx.fillStyle = meterStyle.maxPeakMarkerColor
            ctx.fillRect(0, root.height - 10 - maxPeakHeight, root.indicatorWidth, 1)
        }
    }

    function drawBarStyleDefault(ctx) {
        // On clipping draw full red rectangle
        if (prv.isClipping) {
            ctx.fillStyle = meterStyle.clippedColor
            ctx.fillRect(0, 0, root.indicatorWidth, prv.indicatorHeight)
            return
        }

        // Draw the volume pressure
        const meterHeight = prv.sampleValueToHeight(prv.updatedVolumePressure)
        if (meterHeight > 0) {
            ctx.fillStyle = meterStyle.defaultColor
            ctx.fillRect(0, root.height - 10 - meterHeight, indicatorWidth, meterHeight)
        }

        drawPeakMarkers(ctx)
    }

    function drawBarStyleRMS(ctx) {
        // On clipping draw full red rectangle
        if (prv.isClipping) {
            ctx.fillStyle = meterStyle.clippedColor
            ctx.fillRect(0, 0, indicatorWidth, prv.indicatorHeight)
            return
        }

        var yRMS = prv.sampleValueToHeight(root.currentRMS)
        var yPeak = prv.sampleValueToHeight(root.currentVolumePressure)

        ctx.fillStyle = meterStyle.rmsColor
        ctx.fillRect(0, root.height - 10 - yPeak, root.indicatorWidth, yPeak)

        ctx.fillStyle = meterStyle.rmsOverlayColor
        ctx.fillRect(0, root.height - 10 - yPeak, root.indicatorWidth, yPeak - yRMS)

        drawPeakMarkers(ctx)
    }

    function drawBarStyleGradient(ctx) {
        // Draw the volume pressure
        const meterHeight = prv.sampleValueToHeight(prv.updatedVolumePressure)
        if (meterHeight > 0) {
            ctx.fillStyle = meterStyle.createGradient(ctx, 0, prv.indicatorHeight)
            ctx.fillRect(0, root.height - 10 - meterHeight, indicatorWidth, meterHeight)
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
        drawClippedIndicator(ctx)
        if (prv.needsClear) {
            // Just don´t draw anything else
            prv.needsClear = false
            return
        }
        drawMeterBar(ctx)

        if (root.showRuler) {
            ruler.drawRuler(ctx)
        }
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

    onMeterStyleChanged: {
        requestPaint()
    }

    onIsPlayingChanged: {
        if (root.isPlaying) {
            prv.clipped = false
        }
        else {
            prv.needsClear = true
            prv.recentVolumePressure = []
            prv.recentPeak = -60
            prv.maxPeak = -60
        }
        requestPaint()
    }

    onIsRecordingChanged: {
        if (root.isRecording) {
            prv.clipped = false
            prv.needsClear = true
            prv.recentVolumePressure = []
            prv.recentPeak = -60
            prv.maxPeak = -60
        }
        requestPaint()
    }

    Component.onCompleted: {
        requestPaint()
    }
}
