import QtQuick

Item {
    id: root

    readonly property var edgeId: ({
            Top: 0,
            Left: 1,
            Bottom: 2,
            Right: 3
        })

    required property int edge
    required property bool changeCursorShape

    clip: false

    MouseArea {

        readonly property int margin: 5

        x: -margin
        y: -margin
        width: parent.width + margin * 2
        height: parent.height + margin * 2

        visible: root.changeCursorShape
        hoverEnabled: false
        propagateComposedEvents: true
        onPositionChanged: function (mouse) {
            mouse.accepted = false
        }
        acceptedButtons: Qt.NoButton
        cursorShape: {
            switch (root.edge) {
            case root.edgeId.Top:
            case root.edgeId.Bottom:
                return Qt.SizeVerCursor
            case root.edgeId.Left:
            case root.edgeId.Right:
                return Qt.SizeHorCursor
            default:
                return Qt.ArrowCursor
            }
        }
    }
}
