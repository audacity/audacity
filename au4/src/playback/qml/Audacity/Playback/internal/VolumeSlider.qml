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

        function drawMeter(ctx, originHPos, originVPos, volumePressure) {
            ctx.fillStyle = "#4D4D4D"
            ctx.fillRect(originHPos, originVPos, prv.indicatorWidth , prv.indicatorHeight)

            var isClipping = volumePressure >= root.maxDisplayedVolumePressure

            ctx.fillStyle = isClipping ? "#FF1C1C" : "#666666"
            ctx.fillRect(originHPos + prv.indicatorWidth, originVPos, prv.overloadWidth, prv.indicatorHeight)

            ctx.fillStyle = ui.theme.accentColor
            ctx.fillRect(originHPos, originVPos, prv.divisionPixels * (prv.fullValueRangeLength - Math.abs(volumePressure)), prv.indicatorHeight)
        }

        onPaint: {
            var ctx = bgCanvas.context

            if (!ctx) {
                ctx = getContext("2d")
            }

            ctx.clearRect(0, 0, bgCanvas.height, bgCanvas.width)

            var xPos = prv.handleWidth/2
            var yPos = prv.margin

            drawMeter(ctx, prv.handleWidth/2, yPos, root.leftCurrentVolumePressure)

            yPos += prv.indicatorHeight + prv.spacing

            drawMeter(ctx, xPos, yPos, root.rightCurrentVolumePressure)
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
            color: "white"
            border.color: "black"
            opacity: 0.3
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
