import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Rectangle {

    id: root

    property var context: null

    property alias title: titleLabel.text

    property int trackId: -1
    property int clipIndex: -1

    StyledTextLabel {
        id: titleLabel
        anchors.left: parent.left
        anchors.right: parent.right
    }

    WaveTrackView {
        anchors.top: titleLabel.bottom
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom

        context : root.context
        trackId : root.trackId
        clipIndex: root.clipIndex
    }
}
