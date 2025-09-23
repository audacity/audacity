/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Shapes 1.15
import Muse.Ui
import Audacity.BuiltinEffects

/*
 * A rectangle with upper rounded corners
 */

Shape {
    id: root

    property bool isClipping: false

    signal clicked

    QtObject {
        id: prv

        readonly property int radius: 2
        property color color: root.isClipping ? DynamicsColors.clippingColor : ui.theme.backgroundSecondaryColor
    }

    MouseArea {
        anchors.fill: parent
        cursorShape: Qt.PointingHandCursor
        onClicked: {
            root.clicked()
        }
    }

    ShapePath {
        id: path

        strokeWidth: 1
        strokeColor: ui.theme.strokeColor
        fillColor: prv.color

        startX: prv.radius
        startY: 0

        PathLine {
            x: root.width - prv.radius
            y: 0
        }

        PathArc {
            x: root.width
            y: prv.radius
        }

        PathLine {
            x: root.width
            y: root.height
        }

        PathLine {
            x: 0
            y: root.height
        }

        PathLine {
            x: 0
            y: prv.radius
        }

        PathArc {
            x: path.startX
            y: path.startY
        }
    }
}
