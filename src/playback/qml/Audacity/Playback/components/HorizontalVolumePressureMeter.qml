/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.Playback 1.0

Canvas {
    id: root

    property real currentVolumePressure: -60.0
    property real currentRMS: -60.0
    property real minDisplayedVolumePressure: -60.0
    property real maxDisplayedVolumePressure: 0.0
    
    property int meterStyle: PlaybackMeterStyle.Default

    property bool showRuler: false
    property bool showClippedInfo: true

    property int recentPeakIntervalMiliseconds: 600

    readonly property int overloadWidth: 10

    width: parent.width
    height: root.showRuler ? prv.indicatorHeight + prv.unitsTextWidth : prv.indicatorHeight

    opacity: enabled ? 1.0 : ui.theme.itemOpacityDisabled

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
            gradient.addColorStop(0, gradientColorGreen)
            gradient.addColorStop(0.8, gradientColorYellow)
            gradient.addColorStop(1.0, gradientColorRed)

            return gradient
        }
    }

    QtObject {
        id: prv

        property bool needsClear: false

        property bool isClipping: currentVolumePressure >= maxDisplayedVolumePressure
        property bool clipped: false

        readonly property real indicatorWidth: root.width - root.overloadWidth
        readonly property real indicatorHeight: 6

        // value ranges
        readonly property int fullValueRangeLength: root.maxDisplayedVolumePressure - root.minDisplayedVolumePressure
        readonly property real divisionPixels: prv.indicatorWidth / fullValueRangeLength

        readonly property real leftStepMargin: 4

        readonly property real unitsTextWidth: 14
        readonly property color unitTextColor: Utils.colorWithAlpha(ui.theme.fontPrimaryColor, 0.8)
        readonly property string unitTextFont: {
            var pxSize = String('12px')
            var family = String('\'' + ui.theme.bodyFont.family + '\'')

            return pxSize + ' ' + family
        }

        property real updatedVolumePressure: -60.0
        property real maxPeak: -60.0
        property real recentPeak: -60.0
        property var recentVolumePressure: []

        onUnitTextColorChanged: { root.requestPaint() }
        onUnitTextFontChanged: { root.requestPaint() }

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
            return prv.divisionPixels * (prv.fullValueRangeLength - Math.abs(clampedValue));
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

        function drawRuler(ctx) {
            var originHPos = 0
            var originVPos = root.height - 2

            ctx.clearRect(0, prv.indicatorHeight, root.width, root.height - prv.indicatorHeight)
            ctx.font = prv.unitTextFont

            var currentStrokeHPos = 0
            var fullStep = 12
            var smallStep = 6
            var division = prv.divisionPixels
            var negativeMargin = 8


            ctx.save()
            ctx.fillStyle = prv.unitTextColor

            for (var i = 0; i <= prv.fullValueRangeLength; i+=fullStep) {
                var value = String(root.minDisplayedVolumePressure + i)

                if (i === 0) { // first element
                    value = root.minDisplayedVolumePressure + prv.leftStepMargin
                    currentStrokeHPos = originHPos + negativeMargin
                } else if (i === fullStep) { // second element
                    currentStrokeHPos = originHPos + division * fullStep
                } else {
                    currentStrokeHPos += division * fullStep
                }

                ctx.fillStyle = prv.unitTextColor
                ctx.fillText(value, currentStrokeHPos - (value <= -10 ? negativeMargin : 2), originVPos)
            }
            ctx.restore()
        }
    }

    function resetClipped() {
        prv.clipped = false
        requestPaint()
    }

    function reset() {
        prv.maxPeak = -60
        prv.recentPeak = -60
        prv.recentVolumePressure = []
        prv.updatedVolumePressure = -60

        requestPaint()
    }

    function drawBackground(ctx) {
        ctx.clearRect(0, 0, prv.indicatorWidth, prv.indicatorHeight)

        ctx.fillStyle = meterStyle.meterBackgroundColor
        ctx.fillRect(0, 0, prv.indicatorWidth, prv.indicatorHeight)
    }

    function drawClippedIndicator(ctx) {
        ctx.fillStyle = prv.clipped ? meterStyle.clippedColor : meterStyle.noClippedColor
        ctx.fillRect(prv.indicatorWidth, 0, root.overloadWidth, prv.indicatorHeight)
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
        const recentPeakWidth = prv.sampleValueToHeight(prv.recentPeak)
        if (recentPeakWidth > 0) {
            ctx.fillStyle = meterStyle.getRecentPeakMarkerColor()
            ctx.fillRect(recentPeakWidth, 0, 1, prv.indicatorHeight)
        }
        
        const maxPeakWidth = prv.sampleValueToHeight(prv.maxPeak)
        if (maxPeakWidth > 0) {
            ctx.fillStyle = meterStyle.maxPeakMarkerColor
            ctx.fillRect(maxPeakWidth, 0, 1, prv.indicatorHeight)
        }
    }

    function drawBarStyleDefault(ctx) {
        // On clipping draw full red rectangle   
        if (prv.isClipping) {
            ctx.fillStyle = meterStyle.clippedColor
            ctx.fillRect(0, 0, prv.indicatorWidth, prv.indicatorHeight)
            return
        }

        // Draw the volume pressure
        const meterHeight = prv.sampleValueToHeight(root.currentVolumePressure)
        if (meterHeight > 0) {
            ctx.fillStyle = meterStyle.defaultColor
            ctx.fillRect(0, 0, meterHeight, prv.indicatorHeight)
        }

        drawPeakMarkers(ctx)
    }

    function drawBarStyleRMS(ctx) {
        // On clipping draw full red rectangle
        if (prv.isClipping) {
            ctx.fillStyle = meterStyle.clippedColor
            ctx.fillRect(0, 0, prv.indicatorWidth , prv.indicatorHeight)
            return
        }

        var xRMS = prv.sampleValueToHeight(root.currentRMS)
        var xPeak = prv.sampleValueToHeight(root.currentVolumePressure)

        ctx.fillStyle = meterStyle.rmsColor
        ctx.fillRect(0, 0, xPeak, prv.indicatorHeight)

        ctx.fillStyle = meterStyle.rmsOverlayColor
        ctx.fillRect(xRMS, 0, xPeak - xRMS, prv.indicatorHeight)

        drawPeakMarkers(ctx)
    }

    function drawBarStyleGradient(ctx) {
        const meterHeight = prv.sampleValueToHeight(root.currentVolumePressure)
        if (meterHeight > 0) {
            ctx.fillStyle = meterStyle.createGradient(ctx, prv.indicatorWidth, 0)
            ctx.fillRect(0, 0, meterHeight, prv.indicatorHeight)
        }

        drawPeakMarkers(ctx)
    }

    onPaint: {
        var ctx = root.context

        if (!ctx) {
            ctx = getContext("2d")
        }

        drawBackground(ctx)

        if (root.showClippedInfo)
        {
            drawClippedIndicator(ctx)
        }

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

    Component.onCompleted: {
        requestPaint()
    }
}
