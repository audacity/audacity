import QtQuick
import QtQuick.Shapes

Item {
	id: root
	property color color: "white"
	property real maxSize: 10
	property real minSize: 4
	property int twinkleDuration: 600
	property int delay: Math.random() * 2000

	width: maxSize
	height: maxSize
	opacity: 0

	layer.enabled: true
	layer.samples: 8
	layer.smooth: true

	Shape {
		anchors.fill: parent

		ShapePath {
			strokeWidth: 0
			fillColor: root.color

			PathMove { x: width/2;      y: 0 }
			PathLine { x: width;        y: height/2 }
			PathLine { x: width/2;      y: height }
			PathLine { x: 0;            y: height/2 }
			PathLine { x: width/2;      y: 0 }
		}
	}


	SequentialAnimation {
		running: true
		loops: Animation.Infinite

		PauseAnimation { duration: root.delay }

		ParallelAnimation {
			NumberAnimation { target: root; property: "opacity"; from: 0; to: 1; duration: twinkleDuration * 0.4 }
			NumberAnimation { target: root; property: "scale"; from: 0.6; to: 1.0; duration: twinkleDuration * 0.4
				easing.type: Easing.OutQuad }
		}

		ParallelAnimation {
			NumberAnimation { target: root; property: "opacity"; from: 1; to: 0; duration: twinkleDuration * 0.6 }
			NumberAnimation { target: root; property: "scale"; from: 1.0; to: 0.4; duration: twinkleDuration * 0.6
				easing.type: Easing.InQuad }
		}
	}
}
