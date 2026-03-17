import QtQuick

Item {
    id: root

    readonly property var eEDGE_ID: ({
            Top: 0,
            Left: 1,
            Bottom: 2,
            Right: 3
        })

    required property int edgeId
    required property bool allowCursorShapeChange

    clip: false

    MouseArea {

        readonly property int margin: 5

        x: -margin
        y: -margin
        width: parent.width + margin * 2
        height: parent.height + margin * 2

        visible: root.allowCursorShapeChange
        hoverEnabled: false
        propagateComposedEvents: true
        onPositionChanged: function (mouse) {
            mouse.accepted = false
        }
        acceptedButtons: Qt.NoButton
        cursorShape: {
            switch (root.edgeId) {
            case eEDGE_ID.Top:
            case eEDGE_ID.Bottom:
                return Qt.SizeVerCursor
            case eEDGE_ID.Left:
            case eEDGE_ID.Right:
                return Qt.SizeHorCursor
            default:
                return Qt.ArrowCursor
            }
        }
    }
}
