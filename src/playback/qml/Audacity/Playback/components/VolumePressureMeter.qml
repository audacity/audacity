/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

Canvas {
    id: root

    property real currentVolumePressure: -60.0
    property real minDisplayedVolumePressure: -60.0
    property real maxDisplayedVolumePressure: 0.0

    property real recentPeak: -60
    property real maxPeak: -60

    property bool showRuler: false

    property bool isClipping: currentVolumePressure >= maxDisplayedVolumePressure

    readonly property int overloadWidth: 4

    width: parent.width
    height: root.showRuler ? prv.indicatorHeight + prv.unitsTextWidth : prv.indicatorHeight

    opacity: enabled ? 1.0 : ui.theme.itemOpacityDisabled

    QtObject {
        id: prv

        readonly property real indicatorWidth: root.width - root.overloadWidth
        readonly property real indicatorHeight: 6

        // value ranges
        readonly property int fullValueRangeLength: root.maxDisplayedVolumePressure - root.minDisplayedVolumePressure
        readonly property real divisionPixels: prv.indicatorWidth / fullValueRangeLength

        readonly property real leftStepMargin: 4

        readonly property color meterBackgroundColor: Utils.colorWithAlpha(ui.theme.strokeColor, 0.7)
        readonly property real unitsTextWidth: 12
        readonly property color unitTextColor: Utils.colorWithAlpha(ui.theme.fontPrimaryColor, 0.8)
        readonly property string unitTextFont: {
            var pxSize = String('8px')
            var family = String('\'' + ui.theme.bodyFont.family + '\'')

            return pxSize + ' ' + family
        }

        onMeterBackgroundColorChanged: { root.requestPaint() }
        onUnitTextColorChanged: { root.requestPaint() }
        onUnitTextFontChanged: { root.requestPaint() }
    }

    function drawRuler(ctx, originHPos, originVPos) {
        ctx.clearRect(0, prv.indicatorHeight, root.width, root.height - prv.indicatorHeight)
        ctx.font = prv.unitTextFont

        var currentStrokeHPos = 0
        var fullStep = 12
        var division = prv.divisionPixels
        var negativeMargin = 8

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

            ctx.save()

            ctx.fillStyle = prv.unitTextColor
            ctx.fillText(value, currentStrokeHPos - (value <= -10 ? negativeMargin : 2), originVPos)

            ctx.restore()
        }
    }

    onPaint: {
        var ctx = root.context

        if (!ctx) {
            ctx = getContext("2d")
        }

        ctx.clearRect(0, 0, root.width, prv.indicatorHeight)

        var xPos = 0
        var yPos = 0

        ctx.fillStyle = prv.meterBackgroundColor
        ctx.fillRect(xPos, yPos, prv.indicatorWidth , prv.indicatorHeight)

        var isClipping = root.currentVolumePressure >= root.maxDisplayedVolumePressure
        var clipingColor = "#EF476F"

        ctx.fillStyle = isClipping ? clipingColor : ui.theme.strokeColor
        ctx.fillRect(xPos + prv.indicatorWidth, yPos, root.overloadWidth, prv.indicatorHeight)

        ctx.fillStyle = isClipping ? clipingColor : ui.theme.accentColor
        ctx.fillRect(xPos, yPos, prv.divisionPixels * (prv.fullValueRangeLength - Math.abs(root.currentVolumePressure)), prv.indicatorHeight)

        ctx.fillRect(xPos + root.recentPeak * root.width, yPos, 1, prv.indicatorHeight)

        ctx.fillStyle = prv.unitTextColor
        ctx.fillRect(xPos + root.maxPeak * root.width, yPos, 1, prv.indicatorHeight)

        if (root.showRuler) {
            yPos = root.height - 4
            drawRuler(ctx, xPos, yPos)
        }
    }

    onCurrentVolumePressureChanged: {
        requestPaint()
    }

    onRecentPeakChanged: {
        requestPaint()
    }

    onMaxPeakChanged: {
        requestPaint()
    }

    Component.onCompleted: {
        requestPaint()
    }
}
