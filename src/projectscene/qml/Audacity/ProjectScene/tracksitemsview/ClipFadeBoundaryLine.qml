import QtQuick
import QtQuick.Shapes

Shape {
    id: root

    width: 1

    ShapePath {
        strokeColor: Qt.rgba(0, 0, 0, 0.5)
        strokeWidth: 1
        capStyle: ShapePath.FlatCap
        strokeStyle: ShapePath.DashLine
        dashPattern: [4, 4]
        fillColor: "transparent"

        startX: 0.5
        startY: 0
        PathLine {
            x: 0.5
            y: root.height
        }
    }
}
