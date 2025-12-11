import QtQuick
import Muse.Ui
import Muse.UiComponents

StyledIconLabel {
    id: root

    antialiasing: true

    y: -1 // offset up to avoid too much cropping

    iconCode: IconCode.PLAYHEAD_FILLED

    font.pixelSize: 17
    color: "black"

    StyledIconLabel {
        id: playheadFill

        // inset the Fill Icon by 1px X,Y
        x: 1
        y: 1

        iconCode: IconCode.PLAYHEAD_FILLED

        font.pixelSize: 15
        color: "white"
    }

    // we do some pixel trickery to hide the aliased bottom part and "connect" to the PlayCursorLine
    Rectangle {
        // this is to remove the aliased part in the bottom of the Icon
        id: playheadBottomCenterDot
        width: 1
        height: 2
        x: parent.width / 2
        y: 15
        color: "white"
        antialiasing: true
    }
    Rectangle {
        // this is to remove the aliased part in the bottom of the Icon
        id: playheadBottomCenterDotLeft
        width: 1
        height: 1
        x: (parent.width / 2) - 1
        y: 16
        color: "black"
        antialiasing: true
    }
    Rectangle {
        // this is to remove the aliased part in the bottom of the Icon
        id: playheadBottomCenterDotRight
        width: 1
        height: 1
        x: (parent.width / 2) + 1
        y: 16
        color: "black"
        antialiasing: true
    }
}