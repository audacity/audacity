/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

Dial {
    id: root

    property alias navigation: navCtrl

    property real radius: 16
    property real backgroundHeight: radius + radius * Math.sin(prv.startAngle)

    property bool isPanKnob: false

    property bool accentControl: true

    property alias mouseArea: mouseArea

    implicitWidth: root.radius * 2
    implicitHeight: implicitWidth

    width: implicitWidth
    height: width

    wheelEnabled: true

    from: 0
    to: 1
    value: 0

    signal newValueRequested(real newValue)
    signal increaseRequested()
    signal decreaseRequested()
    signal mouseEntered()
    signal mouseExited()
    signal mousePressed()
    signal mouseReleased()

    QtObject {
        id: prv

        readonly property bool reversed: root.isPanKnob ? root.angle < 0 : false

        readonly property real handlerHeight: radius / 2
        readonly property real handlerWidth: radius / 8

        readonly property real outerArcLineWidth: radius / 5
        readonly property real innerArcLineWidth: radius / 8

        readonly property real startAngle: -140 * (Math.PI/180) - Math.PI/2
        readonly property real endAngle: 140 * (Math.PI/180) - Math.PI/2

        readonly property color valueArcColor: accentControl ? ui.theme.accentColor : Utils.colorWithAlpha(ui.theme.fontPrimaryColor, 0.3)
        readonly property color outerArcColor: Utils.colorWithAlpha(ui.theme.buttonColor, 0.7)
        readonly property color innerArcColor: Utils.colorWithAlpha(ui.theme.fontPrimaryColor, 0.5)

        readonly property real startValueArcAngle: root.isPanKnob ? 0 : -140

        property real prevX: 0
        property real prevY: 0
        property real unclampedValue: -1

        property bool shiftPressed: false
        property bool dragActive: false

        function requestNewValue(newValue) {
            newValue = Math.max(root.from, Math.min(newValue, root.to))

            if (newValue === root.value) {
                return
            }

            root.newValueRequested(newValue)
        }

        onValueArcColorChanged: { backgroundCanvas.requestPaint() }
        onOuterArcColorChanged: { backgroundCanvas.requestPaint() }
        onInnerArcColorChanged: { backgroundCanvas.requestPaint() }
    }

    NavigationControl {
        id: navCtrl
        name: root.objectName != "" ? root.objectName : "KnobControl"
        enabled: root.enabled && root.visible

        accessible.role: MUAccessible.Range
        accessible.visualItem: root

        accessible.value: root.value
        accessible.minimumValue: root.from
        accessible.maximumValue: root.to
        accessible.stepSize: root.stepSize

        onNavigationEvent: function(event) {
            switch(event.type) {
            case NavigationEvent.Left:
                prv.requestNewValue(root.value - root.stepSize)
                event.accepted = true
                break
            case NavigationEvent.Right:
                prv.requestNewValue(root.value + root.stepSize)
                event.accepted = true
                break
            }
        }
    }

    background: Canvas {
        id: backgroundCanvas

        width: root.radius * 2
        height: width

        antialiasing: true

        NavigationFocusBorder {
            navigationCtrl: navCtrl
        }

        onPaint: {
            var ctx = backgroundCanvas.context

            if (!ctx) {
                ctx = getContext("2d")
                ctx.lineCap = "squared"
            }

            ctx.clearRect(0, 0, canvasSize.width, canvasSize.height)
            ctx.lineWidth = prv.outerArcLineWidth

            ctx.strokeStyle = prv.outerArcColor
            ctx.beginPath()
            ctx.arc(width/2, height/2, root.radius - prv.outerArcLineWidth/2, prv.startAngle, prv.endAngle, false)
            ctx.stroke()

            ctx.lineWidth = prv.outerArcLineWidth + 0.5
            ctx.strokeStyle = prv.valueArcColor
            ctx.beginPath()
            ctx.arc(width/2, height/2, root.radius - prv.outerArcLineWidth/2 - 0.25, prv.startValueArcAngle * (Math.PI/180) - Math.PI/2, root.angle * (Math.PI/180) - Math.PI/2, prv.reversed)
            ctx.stroke()

            ctx.lineWidth = prv.innerArcLineWidth
            ctx.strokeStyle = prv.innerArcColor
            ctx.beginPath()
            ctx.arc(width/2, height/2, root.radius - (prv.outerArcLineWidth + prv.innerArcLineWidth/2), 0, Math.PI * 2, false)
            ctx.stroke()
        }
    }

    handle: Rectangle {
        x: root.radius - prv.handlerWidth / 2
        y: prv.outerArcLineWidth + prv.innerArcLineWidth + 2

        height: prv.handlerHeight
        width: prv.handlerWidth
        radius: prv.handlerWidth / 2

        color: ui.theme.fontPrimaryColor
        antialiasing: true

        transformOrigin: Item.Bottom
        rotation: root.angle
    }

    onValueChanged: {
        backgroundCanvas.requestPaint()
    }

    Component.onCompleted: {
        backgroundCanvas.requestPaint()
    }

    onMoved: {
        navigation.requestActiveByInteraction()

        newValueRequested(value)
    }

    MouseArea {
        id: mouseArea
        anchors.fill: parent
        hoverEnabled: true
        onDoubleClicked: {
            prv.requestNewValue(0)
        }

        // The MouseArea steals mouse press events from the slider.
        // There is really no way to prevent that.
        // (if you set mouse.accepted to false in the onPressed handler,
        // the MouseArea won't receive doubleClick events).
        // So we have to reimplement the dragging behaviour.
        // That gives us the opportunity to do it better than Qt.
        // We will allow both dragging vertically and horizontally.

        preventStealing: true // Don't let a Flickable steal the mouse

        onPressed: function(mouse) {
            prv.prevX = mouse.x
            prv.prevY = mouse.y
            prv.dragActive = true
            mousePressed()
        }

        onEntered: {
            mouseEntered()
        }

        onExited: {
            // Consider the mouse is still inside,
            // if we are dragging outside
            if (!prv.dragActive) {
                mouseExited()
            }
        }

        onReleased: {
            prv.dragActive = false
            prv.unclampedValue = -1
            if (!containsMouse) {
                mouseExited()
            }
            mouseReleased()
        }

        onPositionChanged: function(mouse)  {
            if (prv.dragActive) {

                if ((mouse.modifiers & (Qt.ShiftModifier))) {
                    if (!prv.shiftPressed) {
                        prv.shiftPressed = true
                        prv.unclampedValue = -1
                    }
                } else {
                    if (prv.shiftPressed) {
                        prv.shiftPressed = false
                        prv.unclampedValue = -1
                    }
                }

                const dx = mouse.x - prv.prevX
                const dy = mouse.y - prv.prevY
                let dist = Math.sqrt(dx * dx + dy * dy)
                if ((mouse.modifiers & (Qt.ShiftModifier))) {
                    dist /= 3
                }
                const sgn = (dy < dx) ? 1 : -1
                const span = root.to - root.from
                const newValue = (prv.unclampedValue === -1 ? root.value : prv.unclampedValue) + span * dist / 200 * sgn

                prv.prevX = mouse.x
                prv.prevY = mouse.y
                prv.unclampedValue = newValue

                prv.requestNewValue(newValue)
            }
        }

        // We also listen for wheel events here, but for a different reason:
        // Qml Dial has a bug that it doesn't emit moved() when the value is changed through a wheel event.
        // So when we see a wheel event, we let the dial handle it, but we will account for emitting the signal.
        onWheel: function(wheel) {
            wheel.accepted = false
            Qt.callLater(function() { root.newValueRequested(Math.round(value)) })
        }
    }
}
