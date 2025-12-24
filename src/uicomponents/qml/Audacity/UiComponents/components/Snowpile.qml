import QtQuick
import QtQuick.Shapes

Item {
    id: root
    width: parent ? parent.width : 800
    height: 120

    property real maxPileHeight: 80
    property real buildUpDuration: 600000
    property real currentHeight: 0

    Component.onCompleted: {
        buildAnimation.start()
    }

    NumberAnimation {
        id: buildAnimation
        target: root
        property: "currentHeight"
        from: 0
        to: root.maxPileHeight
        duration: root.buildUpDuration
        easing.type: Easing.OutQuad
    }

    Shape {
        id: snowShape
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        height: root.currentHeight + 20
        antialiasing: true
        smooth: true
        layer.enabled: true
        layer.smooth: true
        layer.samples: 4

        ShapePath {
            id: snowPath
            fillColor: '#e1ebfe'
            strokeColor: '#d6e9fa'
            strokeWidth: 1

            startX: 0
            startY: snowShape.height

            PathLine { x: 0; y: snowShape.height - root.currentHeight * 0.7 }


            PathCurve { x: root.width * 0.05; y: snowShape.height - root.currentHeight * 0.75 }
            PathCurve { x: root.width * 0.1; y: snowShape.height - root.currentHeight * 0.9 }
            PathCurve { x: root.width * 0.15; y: snowShape.height - root.currentHeight * 0.85 }
            PathCurve { x: root.width * 0.2; y: snowShape.height - root.currentHeight * 0.95 }
            PathCurve { x: root.width * 0.25; y: snowShape.height - root.currentHeight * 0.88 }
            PathCurve { x: root.width * 0.3; y: snowShape.height - root.currentHeight * 1.0 }
            PathCurve { x: root.width * 0.35; y: snowShape.height - root.currentHeight * 0.92 }
            PathCurve { x: root.width * 0.4; y: snowShape.height - root.currentHeight * 0.87 }
            PathCurve { x: root.width * 0.45; y: snowShape.height - root.currentHeight * 0.96 }
            PathCurve { x: root.width * 0.5; y: snowShape.height - root.currentHeight * 0.9 }
            PathCurve { x: root.width * 0.55; y: snowShape.height - root.currentHeight * 0.98 }
            PathCurve { x: root.width * 0.6; y: snowShape.height - root.currentHeight * 0.86 }
            PathCurve { x: root.width * 0.65; y: snowShape.height - root.currentHeight * 0.93 }
            PathCurve { x: root.width * 0.7; y: snowShape.height - root.currentHeight * 0.88 }
            PathCurve { x: root.width * 0.75; y: snowShape.height - root.currentHeight * 0.97 }
            PathCurve { x: root.width * 0.8; y: snowShape.height - root.currentHeight * 0.84 }
            PathCurve { x: root.width * 0.85; y: snowShape.height - root.currentHeight * 0.91 }
            PathCurve { x: root.width * 0.9; y: snowShape.height - root.currentHeight * 0.82 }
            PathCurve { x: root.width * 0.95; y: snowShape.height - root.currentHeight * 0.78 }
            PathCurve { x: root.width; y: snowShape.height - root.currentHeight * 0.7 }

            PathLine { x: root.width; y: snowShape.height }
            PathLine { x: 0; y: snowShape.height }
        }
    }


    Shape {
        id: shadowLayer
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        height: root.currentHeight + 20
        opacity: 0.3
        antialiasing: true
        smooth: true
        layer.enabled: true
        layer.smooth: true
        layer.samples: 4

        ShapePath {
            fillColor: '#b6cbe9'
            strokeWidth: 0

            startX: 0
            startY: shadowLayer.height

            PathLine { x: 0; y: shadowLayer.height - root.currentHeight * 0.5 }
            PathCurve { x: root.width * 0.1; y: shadowLayer.height - root.currentHeight * 0.6 }
            PathCurve { x: root.width * 0.25; y: shadowLayer.height - root.currentHeight * 0.55 }
            PathCurve { x: root.width * 0.4; y: shadowLayer.height - root.currentHeight * 0.65 }
            PathCurve { x: root.width * 0.5; y: shadowLayer.height - root.currentHeight * 0.58 }
            PathCurve { x: root.width * 0.65; y: shadowLayer.height - root.currentHeight * 0.62 }
            PathCurve { x: root.width * 0.8; y: shadowLayer.height - root.currentHeight * 0.52 }
            PathCurve { x: root.width * 0.95; y: shadowLayer.height - root.currentHeight * 0.48 }
            PathLine { x: root.width; y: shadowLayer.height }
            PathLine { x: 0; y: shadowLayer.height }
        }
    }


    Repeater {
        model: 10

        Sparkle {
            required property int index

            x: Math.random() * root.width
            y: root.height - root.currentHeight * (Math.random() * 0.4 + 0.5)

            color: "white"
            maxSize: 4
            minSize: 1
            twinkleDuration: 800

            visible: root.currentHeight > 15
            scale: 0.4
        }
    }


    function reset() {
        buildAnimation.stop()
        currentHeight = 0
    }

    function startBuilding() {
        buildAnimation.start()
    }

    function setHeight(h) {
        buildAnimation.stop()
        currentHeight = Math.min(h, maxPileHeight)
    }
}
