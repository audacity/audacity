import QtQuick
import Muse.Ui
import Muse.UiComponents

StyledIconLabel {
    id: root

    readonly property real lineX: Math.round(width / 2 * prv.devicePixelRatio) / prv.devicePixelRatio

    QtObject {
        id: prv

        readonly property real devicePixelRatio: Screen.devicePixelRatio > 0 ? Screen.devicePixelRatio : 1
        readonly property real pixel: Math.max(1, Math.round(devicePixelRatio)) / devicePixelRatio
    }

    antialiasing: true

    y: -1 // offset up to avoid too much cropping

    iconCode: IconCode.PLAYHEAD_FILLED

    font.pixelSize: 17
    color: ui.theme.extra["black_color"]

    StyledIconLabel {
        id: playheadFill

        // inset the Fill Icon by 1px X,Y
        x: 1
        y: 1

        iconCode: IconCode.PLAYHEAD_FILLED

        font.pixelSize: 15
        color: ui.theme.extra["white_color"]
    }

    // we do some pixel trickery to hide the aliased bottom part and "connect" to the PlayCursorLine
    Rectangle {
        // this is to remove the aliased part in the bottom of the Icon
        id: playheadBottomCenterDot
        width: prv.pixel
        height: 2
        x: root.lineX
        y: 15
        color: ui.theme.extra["white_color"]
        antialiasing: true
    }
    Rectangle {
        // this is to remove the aliased part in the bottom of the Icon
        id: playheadBottomCenterDotLeft
        width: prv.pixel
        height: 1
        x: root.lineX - prv.pixel
        y: 16
        color: ui.theme.extra["black_color"]
        antialiasing: true
    }
    Rectangle {
        // this is to remove the aliased part in the bottom of the Icon
        id: playheadBottomCenterDotRight
        width: prv.pixel
        height: 1
        x: root.lineX + prv.pixel
        y: 16
        color: ui.theme.extra["black_color"]
        antialiasing: true
    }
}
