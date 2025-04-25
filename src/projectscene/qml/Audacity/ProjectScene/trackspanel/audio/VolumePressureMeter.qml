/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.UiComponents 1.0
import Muse.Ui 1.0

Canvas {
    id: root

    enum Style {
        Solid,
        Gradient
    }

    property real currentVolumePressure: -60.0
    property real updatedVolumePressure: -60.0
    property real maxPeak: -60.0
    property real recentPeak: -60.0
    property var recentVolumePressure: []
    property real minDisplayedVolumePressure: -60.0
    property real maxDisplayedVolumePressure: 0.0
    property bool isPlaying: false

    property real indicatorWidth
    property bool showRuler: false
    property int style: VolumePressureMeter.Style.Solid
    property color meterColor: "#7689E6" // TODO: Use the track color

    property bool isClipping: updatedVolumePressure >= maxDisplayedVolumePressure
    property bool wasClipped: false

    width: root.showRuler ? indicatorWidth + 20 : indicatorWidth

    QtObject {
        id: prv

        property var gradient: null
        readonly property int overloadHeight: 4

        readonly property real indicatorHeight: root.height - prv.overloadHeight - 6


        // value ranges
        readonly property int fullValueRangeLength: root.maxDisplayedVolumePressure - root.minDisplayedVolumePressure
        readonly property real heightPerUnit: (prv.indicatorHeight - prv.overloadHeight) / fullValueRangeLength

        readonly property real unitsTextWidth: 12
        readonly property color unitTextColor: Utils.colorWithAlpha(ui.theme.fontPrimaryColor, 0.8)
        readonly property string unitTextFont: {
            var pxSize = String('8px')
            var family = String('\'' + ui.theme.bodyFont.family + '\'')

            return pxSize + ' ' + family
        }

        onUnitTextColorChanged: { prv.rulerNeedsPaint = true; root.requestPaint() }
        onUnitTextFontChanged: { prv.rulerNeedsPaint = true; root.requestPaint() }

        // strokes
        readonly property real strokeHorizontalMargin: 2
        readonly property real longStrokeHeight: 1
        readonly property real longStrokeWidth: 5
        readonly property color longStrokeColor: Utils.colorWithAlpha(ui.theme.fontPrimaryColor, 0.5)
        readonly property real shortStrokeHeight: 1
        readonly property real shortStrokeWidth: 2
        readonly property color shortStrokeColor: Utils.colorWithAlpha(ui.theme.fontPrimaryColor, 0.3)

        onLongStrokeColorChanged: { prv.rulerNeedsPaint = true; root.requestPaint() }
        onShortStrokeColorChanged: { prv.rulerNeedsPaint = true; root.requestPaint() }

        property bool rulerNeedsPaint: true
        property bool needsClear: false
    }

    function onTapped() {
        root.maxPeak = -60
        root.recentPeak = -60
        root.wasClipped = false
        root.recentVolumePressure = []
        root.updatedVolumePressure = -60
        requestPaint()
    }

    function updateRecentPeak(currentValue) {
        const now = Date.now()

        root.recentVolumePressure.push({ value: currentValue, time: now })

        const recentPeakStartInterval = now - 600
        let cutoffIndex = -1

        for (let i = 0; i < root.recentVolumePressure.length; i++) {
            if (root.recentVolumePressure[i].time < recentPeakStartInterval) {
                cutoffIndex = i
            }
            else {
                break
            }
        }

        if (cutoffIndex !== -1) {
            root.recentVolumePressure.splice(0, cutoffIndex)
        }

        // Calculate the recent peak
        let recentPeak = -60
        for (let i = 0; i < root.recentVolumePressure.length; i++) {
            if (root.recentVolumePressure[i].value > recentPeak) {
                recentPeak = root.recentVolumePressure[i].value
            }
        }

        return recentPeak
    }

    // Rounding up fullStep value to the predefined one,
    // to avoid getting funny intervals like 3, or 7
    function roundUpToFixedValue(value) {
        // full and small step is
        // a number units per each respective notch on the ruler
        const steps = [
            { fullStep: 1, smallStep: 0 },
            { fullStep: 2, smallStep: 1 },
            { fullStep: 5, smallStep: 1 },
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

    // Draws a rectangle rounded at the top, bottom or both
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

    function getMeterFillStyle(ctx) {
        if (root.style === VolumePressureMeter.Style.Gradient) {
            if (!prv.gradient) {
                // Preparing the gradient to draw the volume pressure
                prv.gradient = ctx.createLinearGradient(0, prv.indicatorHeight - prv.overloadHeight, 0, prv.overloadHeight)
                prv.gradient.addColorStop(0.0, "#26E386")
                prv.gradient.addColorStop(0.55, "#CBED41")
                prv.gradient.addColorStop(0.80, "#FC8226")
            }
            return prv.gradient
        }
        return root.meterColor
    }

    function drawRuler(ctx, originHPos, originVPos) {
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
            ctx.fillStyle = prv.shortStrokeColor
            ctx.fillRect(originHPos, vPos,
                         prv.shortStrokeWidth,
                         prv.shortStrokeHeight)
        }

        // Drawing full steps
        for (let j = 0; j <= fullStepCount; j++) {
            const vPos = originVPos + prv.heightPerUnit * fullStep * j;
            ctx.fillStyle = prv.longStrokeColor
            ctx.fillRect(originHPos, vPos,
                         prv.longStrokeWidth,
                         prv.longStrokeHeight)

            let textHPos = originHPos + prv.longStrokeWidth + prv.strokeHorizontalMargin
            ctx.fillStyle = prv.unitTextColor
            ctx.fillText(fullStep * j, textHPos, vPos + 2)
        }

        prv.rulerNeedsPaint = false
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

        ctx.clearRect(0, 0, indicatorWidth, prv.indicatorHeight)

        // Filling the background of the meter
        drawRoundedRect(ctx, ui.theme.strokeColor, 0, 0, indicatorWidth, prv.indicatorHeight, 2, "both")

        // Drawing the Overload indicator
        const overloadStyle = root.wasClipped ? "#EF476F" : ui.theme.buttonColor
        drawRoundedRect(ctx, overloadStyle, 0, 0, indicatorWidth, prv.overloadHeight, 2, "top")

        // Clamping the current volume pressure
        const volumePressure = Math.max(minDisplayedVolumePressure,
                                    Math.min(updatedVolumePressure, maxDisplayedVolumePressure));
        // calculating the current volume height depending on the panel height
        const meterHeight = prv.heightPerUnit * (volumePressure - root.minDisplayedVolumePressure)

        const volumePressurePeak = Math.max(minDisplayedVolumePressure,
                                    Math.min(root.maxPeak, maxDisplayedVolumePressure));

        const meterPeakHeight = prv.heightPerUnit * (volumePressurePeak - root.minDisplayedVolumePressure)

        const volumePressureRecentPeak = Math.max(minDisplayedVolumePressure,
                                    Math.min(root.recentPeak, maxDisplayedVolumePressure));
        
        const meterRecentPeakHeight = prv.heightPerUnit * (volumePressureRecentPeak - root.minDisplayedVolumePressure)

        if (!prv.needsClear) {
            if (root.isClipping) {
                drawRoundedRect(ctx, "#EF476F", 0, 0, indicatorWidth, prv.indicatorHeight, 2, "both")
            }
            else {
                if (meterHeight > 0) {
                    // Drawing the volume pressure
                    drawRoundedRect(ctx, ui.theme.accentColor,
                                    0, root.height - 10 - meterHeight,
                                    indicatorWidth, meterHeight,
                                    2, "bottom")
                }

                drawRoundedRect(ctx, ui.theme.accentColor,
                                0, root.height - 10 - meterRecentPeakHeight,
                                indicatorWidth, 1,
                                0, "none")

                drawRoundedRect(ctx, prv.unitTextColor,
                                0, root.height - 10 - meterPeakHeight,
                                indicatorWidth, 1,
                                0, "none")
                

                if (prv.rulerNeedsPaint) {
                    var originVPos = prv.overloadHeight
                    var originHPos = indicatorWidth + prv.strokeHorizontalMargin

                    drawRuler(ctx, originHPos, originVPos)
                }
            }
        }
        else {
            prv.needsClear = false
        }
    }

    onHeightChanged: {
        // Gradient and the ruler need to be updated
        prv.rulerNeedsPaint = true
        prv.gradient = null
        requestPaint();
    }

    onIsClippingChanged: {
        if (!root.wasClipped && root.isClipping) {
            root.wasClipped = true
            requestPaint()
        }
    }

    onCurrentVolumePressureChanged: {
        root.updatedVolumePressure = root.currentVolumePressure
        root.recentPeak = updateRecentPeak(currentVolumePressure)
        requestPaint()
    }

    onIsPlayingChanged: {
        if (!root.isPlaying) {
            prv.needsClear = true
            root.recentVolumePressure = []
            root.recentPeak = -60
            root.maxPeak = -60
        }
        else {
            root.wasClipped = false
        }
        requestPaint()
    }

    Component.onCompleted: {
        prv.rulerNeedsPaint = true
        requestPaint()
    }
}
