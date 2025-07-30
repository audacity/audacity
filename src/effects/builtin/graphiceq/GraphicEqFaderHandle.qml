import QtQuick 2.15
import Muse.Ui

Rectangle {
    width: 16
    height: 32
    radius: 2
    color: ui.theme.backgroundPrimaryColor
    border.color: ui.theme.isDark ? "#8C9196" : "#5B5B5F"
    border.width: 1

    Rectangle {
        id: fingerGrip // That horizontal dent in the middle of the fader handle

        width: 10
        height: 1
        color: ui.theme.fontPrimaryColor
        y: 16
        anchors.horizontalCenter: parent.horizontalCenter
    }
}
