import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Rectangle {

    id: root

    property alias context: waveView.context
    property alias title: titleLabel.text
    property alias clipKey: waveView.clipKey

    signal positionChanged(x : double)

    radius: 4
    color: ui.theme.backgroundPrimaryColor
    border.width: 1
    border.color: ui.theme.strokeColor
    clip: true

    Drag.active: dragArea.drag.active

    Rectangle {
        id: header
        anchors.left: parent.left
        anchors.right: parent.right
        height: 20

        color: ui.theme.backgroundSecondaryColor

        StyledTextLabel {
            id: titleLabel
            anchors.fill: parent
        }

        MouseArea {
            id: dragArea
            anchors.fill: parent

            cursorShape: Qt.OpenHandCursor
            drag.target: root
            drag.axis: Drag.XAxis

            onReleased: root.positionChanged(root.x)
        }
    }

    WaveView {
        id: waveView
        anchors.top: header.bottom
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        anchors.margins: 1
    }
}
