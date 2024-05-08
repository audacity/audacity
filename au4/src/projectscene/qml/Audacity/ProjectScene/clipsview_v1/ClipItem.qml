import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Rectangle {

    id: root

    property var context: null

    property alias title: titleLabel.text
    property alias clipKey: waveView.clipKey

    radius: 4
    color: ui.theme.backgroundPrimaryColor
    border.width: 1
    border.color: ui.theme.strokeColor
    clip: true

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
    }

    WaveView {
        id: waveView
        anchors.top: header.bottom
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        anchors.margins: 1

        context : root.context
    }
}
