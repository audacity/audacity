/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.Ui

Item {
    id: root

    property AbstractNavigation navigationCtrl: null

    property color outerColor: ui.theme.fontSecondaryColor
    property color innerColor: ui.theme.isDark ? ui.theme.extra["white_color"] : ui.theme.extra["black_color"]

    property real radius: 0

    anchors.fill: parent

    visible: navigationCtrl ? navigationCtrl.highlight : false

    Rectangle {
        anchors.fill: parent
        anchors.margins: -3

        color: "transparent"
        radius: root.radius + 3

        border.color: root.outerColor
        border.width: 2
    }

    Rectangle {
        anchors.fill: parent
        anchors.margins: -2

        color: "transparent"
        radius: root.radius + 2

        border.color: root.innerColor
        border.width: 2
    }
}
