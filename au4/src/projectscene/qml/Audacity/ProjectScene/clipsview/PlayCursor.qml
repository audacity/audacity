import QtQuick
import Muse.Ui
import Muse.UiComponents

Rectangle {

    id: root

    property color borderColor: "#000000"
    property int borderWidth: 1
    property bool timelinePressed: false

    signal setPlaybackPosition(real x)
    signal playCursorMousePositionChanged(real x)

    Rectangle {
        id: cursor

        x: -width / 2
        width: 3
        height: root.height
        color: "#ffffff"


        // draw borders without top one
        Rectangle {
            id: leftBorder
            color: borderColor
            width: borderWidth
            height: parent.height
            anchors.left: parent.left
        }
        Rectangle {
            id: rightBorder
            color: borderColor
            width: borderWidth
            height: parent.height
            anchors.right: parent.right
        }
        Rectangle {
            id: bottomBorder
            color: borderColor
            width: root.width
            height: borderWidth
            anchors.bottom: parent.bottom
        }
    }

    StyledIconLabel {
        id: playheadIcon

        x: -(playheadIcon.width) / 2 - 0.5
        y: -playheadIcon.height
        z: 1

        iconCode: IconCode.PLAYHEAD_FILLED

        font.pixelSize: 17
        color: "black"

        StyledIconLabel {
            id: playheadFill

            x: 1.1
            y: 0.9

            iconCode: IconCode.PLAYHEAD_FILLED

            font.pixelSize: 15
            color: "white"
        }

        MouseArea {
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: pressed || timelinePressed ? Qt.ClosedHandCursor : Qt.OpenHandCursor

            onPositionChanged: function(e) {
                var ix = mapToItem(root.parent, e.x, e.y).x
                if (pressed) {
                    setPlaybackPosition(ix)
                }
                playCursorMousePositionChanged(ix)
            }
        }

    }
}
