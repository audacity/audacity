/*
* Audacity: A Digital Audio Editor
*/

import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

Slider {
    id: root

    property real leftCurrentVolumePressure: -60.0
    property real leftRecentPeak: -60
    property real leftMaxPeak: -60

    property real rightCurrentVolumePressure: -60.0
    property real rightRecentPeak: -60
    property real rightMaxPeak: -60

    property real maxDisplayedVolumePressure: 0.0

    property real volumeLevel: 0.0
    property real readableVolumeLevel: Math.round(root.volumeLevel * 10) / 10

    property alias navigation: navCtrl

    signal volumeLevelMoved(var level)

    from: -60
    to: 0
    value: root.volumeLevel
    stepSize: 0.1
    orientation: Qt.Horizontal
    wheelEnabled: true

    signal increaseRequested()
    signal decreaseRequested()

    QtObject {
        id: prv

        property int margin: 2
        readonly property int spacing: 2

        property var gradient: null
        readonly property int overloadWidth: 4

        readonly property real indicatorWidth: root.width - handleWidth - overloadWidth
        readonly property real indicatorHeight: 6

        readonly property real rulerLineWidth: root.width - handleWidth
        readonly property real rulerLineHeight: 2

        readonly property int rulerYPos: margin + indicatorHeight + spacing / 2

        readonly property int fullValueRangeLength: Math.abs(root.from) + Math.abs(root.to)
        readonly property real divisionPixels: rulerLineWidth / fullValueRangeLength

        readonly property color meterBackgroundColor: Utils.colorWithAlpha(ui.theme.strokeColor, 0.7)
        readonly property color unitTextColor: ui.theme.fontPrimaryColor
        readonly property string unitTextFont: {
            var pxSize = String('8px')
            var family = String('\'' + ui.theme.bodyFont.family + '\'')

            return pxSize + ' ' + family
        }

        onMeterBackgroundColorChanged: { bgCanvas.requestPaint() }
        onUnitTextColorChanged: { bgCanvas.requestPaint() }
        onUnitTextFontChanged: { bgCanvas.requestPaint() }

        readonly property real handleWidth: 14
        readonly property real handleHeight: 14

        property real dragStartOffset: 0.0
    }

    NavigationControl {
        id: navCtrl
        name: root.objectName !== "" ? root.objectName : "VolumeSlider"
        enabled: root.enabled && root.visible

        accessible.role: MUAccessible.Range
        accessible.visualItem: root

        accessible.value: root.readableVolumeLevel
        accessible.minimumValue: root.from
        accessible.maximumValue: root.to
        accessible.stepSize: root.stepSize

        onNavigationEvent: function(event) {
            switch(event.type) {
            case NavigationEvent.Left:
                root.decreaseRequested()
                event.accepted = true
                break
            case NavigationEvent.Right:
                root.increaseRequested()
                event.accepted = true
                break
            }
        }
    }

    background: Canvas {
        id: bgCanvas

        height: root.height
        width: root.width

        NavigationFocusBorder {
            navigationCtrl: navCtrl
        }

        function drawMeter(ctx, originHPos, originVPos, volumePressure, recentPeak, maxPeak) {
            ctx.fillStyle = prv.meterBackgroundColor
            ctx.fillRect(originHPos, originVPos, prv.indicatorWidth , prv.indicatorHeight)

            var isClipping = volumePressure >= root.maxDisplayedVolumePressure
            var clipingColor = "#EF476F"

            ctx.fillStyle = isClipping ? clipingColor : ui.theme.strokeColor
            ctx.fillRect(originHPos + prv.indicatorWidth, originVPos, prv.overloadWidth, prv.indicatorHeight)

            ctx.fillStyle = isClipping ? clipingColor : ui.theme.accentColor
            ctx.fillRect(originHPos, originVPos, prv.divisionPixels * (prv.fullValueRangeLength - Math.abs(volumePressure)), prv.indicatorHeight)

            ctx.fillRect(originHPos + recentPeak * prv.rulerLineWidth, originVPos, 1, prv.indicatorHeight)

            ctx.fillStyle = prv.unitTextColor
            ctx.fillRect(originHPos + maxPeak * prv.rulerLineWidth, originVPos, 1, prv.indicatorHeight)
        }

        function drawRuler(ctx, originHPos, originVPos) {
            ctx.font = prv.unitTextFont

            var currentStrokeHPos = 0
            var fullStep = 12
            var smallStep = 6

            for (var i = 0; i <= prv.fullValueRangeLength; i+=fullStep) {
                var division = prv.divisionPixels

                if (i === 0) {
                    currentStrokeHPos = originHPos
                } else {
                    currentStrokeHPos += division * fullStep
                }

                ctx.save()

                ctx.fillStyle = prv.unitTextColor
                var value = String(root.from + i)
                ctx.fillText(value, currentStrokeHPos - (value <= -10 ? 8 : (value < 0 ? 6 : 2)), originVPos)

                ctx.restore()
            }
        }

        onPaint: {
            var ctx = bgCanvas.context

            if (!ctx) {
                ctx = getContext("2d")
            }

            ctx.clearRect(0, 0, bgCanvas.height, bgCanvas.width)

            var xPos = prv.handleWidth/2
            var yPos = prv.margin

            drawMeter(ctx, xPos, yPos, root.leftCurrentVolumePressure, root.leftRecentPeak, root.leftMaxPeak)

            yPos += prv.indicatorHeight + prv.spacing
            drawMeter(ctx, xPos, yPos, root.rightCurrentVolumePressure, root.rightRecentPeak, root.rightMaxPeak)

            yPos = bgCanvas.height
            drawRuler(ctx, xPos, yPos)
        }
    }

    handle: Item {
        x: root.position * prv.rulerLineWidth
        y: prv.rulerYPos - prv.handleHeight / 2
        implicitWidth: prv.handleWidth
        implicitHeight: prv.handleHeight

        MouseArea {
            anchors.fill: parent

            onDoubleClicked: {
                // Double click resets the volume
                root.volumeLevelMoved(0.0)
            }

            // The MouseArea steals mouse press events from the slider.
            // There is really no way to prevent that.
            // (if you set mouse.accepted to false in the onPressed handler,
            // the MouseArea won't receive doubleClick events).
            // So we use this workaround.

            preventStealing: true // Don't let a Flickable steal the mouse

            onPressed: function(mouse) {
                prv.dragStartOffset = mouse.x
            }

            onPositionChanged: function(mouse)  {
                let mousePosInRoot = mapToItem(root, mouse.x - prv.dragStartOffset, 0).x
                let newPosZeroToOne = mousePosInRoot / prv.rulerLineWidth

                let newPosClamped = Math.max(0.0, Math.min(newPosZeroToOne, 1.0))
                let localNewValue = root.valueAt(newPosClamped)
                root.volumeLevelMoved(localNewValue)
            }
        }

        Rectangle {
            id: handleRect
            anchors.fill: parent
            radius: width / 2
            color: "transparent"
            border.color: ui.theme.fontPrimaryColor

            Rectangle {
                anchors.fill: parent
                anchors.margins: 1
                radius: width / 2
                color: ui.theme.backgroundPrimaryColor
                opacity: 0.3
            }

        }
    }

    onMoved: {
        navigation.requestActiveByInteraction()

        root.volumeLevelMoved(value)
    }

    Component.onCompleted: {
        bgCanvas.requestPaint()
    }
}
