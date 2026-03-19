import QtQuick

Rectangle {
    id: root

    readonly property var eCORNER_ID: ({
            TopLeft: 0,
            TopRight: 1,
            BottomLeft: 2,
            BottomRight: 3
        })

    property int size: 6
    required property int cornerId
    required property bool allowCursorShapeChange

    color: "white"
    border.color: "black"
    border.width: 1
    width: size
    height: size

    MouseArea {
        id: mouseArea

        readonly property int margin: 5

        x: -margin
        y: -margin
        width: parent.width + margin * 2
        height: parent.height + margin * 2

        hoverEnabled: false
        visible: root.allowCursorShapeChange
        acceptedButtons: Qt.NoButton
        cursorShape: {
            switch (root.cornerId) {
            case eCORNER_ID.TopLeft:
                return Qt.SizeFDiagCursor
            case eCORNER_ID.TopRight:
                return Qt.SizeBDiagCursor
            case eCORNER_ID.BottomLeft:
                return Qt.SizeBDiagCursor
            case eCORNER_ID.BottomRight:
                return Qt.SizeFDiagCursor
            default:
                return Qt.BusyCursor
            }
        }
    }
}
