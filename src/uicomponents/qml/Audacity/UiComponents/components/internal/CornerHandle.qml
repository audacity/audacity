import QtQuick

Rectangle {
    id: root

    readonly property var cornerId: ({
            TopLeft: 0,
            TopRight: 1,
            BottomLeft: 2,
            BottomRight: 3
        })

    property int size: 6
    required property int corner
    required property bool changeCursorShape

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
        visible: root.changeCursorShape
        acceptedButtons: Qt.NoButton
        cursorShape: {
            switch (root.corner) {
            case root.cornerId.TopLeft:
                return Qt.SizeFDiagCursor
            case root.cornerId.TopRight:
                return Qt.SizeBDiagCursor
            case root.cornerId.BottomLeft:
                return Qt.SizeBDiagCursor
            case root.cornerId.BottomRight:
                return Qt.SizeFDiagCursor
            default:
                return Qt.BusyCursor
            }
        }
    }
}
