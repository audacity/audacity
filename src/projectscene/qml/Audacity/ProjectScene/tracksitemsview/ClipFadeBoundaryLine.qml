import QtQuick
import QtQuick.Shapes

import Muse.Ui

Shape {
    id: root

    width: 1

    //! Fills the gaps between the dashes with white, e.g. while the fade is dragged
    property bool highlighted: false

    QtObject {
        id: prv
        readonly property real dashLength: 4
    }

    ShapePath {
        strokeColor: root.highlighted ? ui.theme.extra["white_color"] : "transparent"
        strokeWidth: 1
        capStyle: ShapePath.FlatCap
        strokeStyle: ShapePath.DashLine
        dashPattern: [prv.dashLength, prv.dashLength]
        dashOffset: prv.dashLength
        fillColor: "transparent"

        startX: 0.5
        startY: 0
        PathLine {
            x: 0.5
            y: root.height
        }
    }

    ShapePath {
        strokeColor: Qt.rgba(0, 0, 0, 0.5)
        strokeWidth: 1
        capStyle: ShapePath.FlatCap
        strokeStyle: ShapePath.DashLine
        dashPattern: [prv.dashLength, prv.dashLength]
        fillColor: "transparent"

        startX: 0.5
        startY: 0
        PathLine {
            x: 0.5
            y: root.height
        }
    }
}
