import QtQuick
import Muse.Ui
import Muse.UiComponents


Item {
    id: playCursorHead

    property bool timelinePressed: false

    signal setPlaybackPosition(real x)
    signal playCursorMousePositionChanged(real x)

    StyledIconLabel {
        id: playheadIcon

        x: -(playheadIcon.width) / 2 - 0.5

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
                var ix = mapToItem(content, e.x, e.y).x
                if (pressed) {
                    setPlaybackPosition(ix)
                }
                playCursorMousePositionChanged(ix)
            }
        }
    }
}
