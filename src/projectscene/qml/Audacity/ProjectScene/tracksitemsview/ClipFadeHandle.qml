import QtQuick

import Muse.Ui

Item {
    id: root

    required property bool isFadeIn

    readonly property alias containsMouse: mouseArea.containsMouse

    ClipHandleIcon {
        anchors.centerIn: parent

        hovered: mouseArea.containsMouse
        iconCode: root.isFadeIn ? IconCode.FADE_HANDLE_IN : IconCode.FADE_HANDLE_OUT
        color: ui.theme.extra["black_color"]
        style: Text.Outline
        styleColor: ui.theme.extra["white_color"]
    }

    MouseArea {
        id: mouseArea

        anchors.fill: parent

        hoverEnabled: true
        cursorShape: Qt.SizeHorCursor
    }
}
