/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Controls

import Muse.UiComponents

import Audacity.ProjectScene

StyledListView {
    id: root

    required property TracksViewStateModel tracksViewState

    readonly property int listHeaderHeight: 2

    property real lockedVerticalScrollPosition
    property bool verticalScrollLocked: root.tracksViewState.tracksVerticalScrollLocked

    interactive: false

    ScrollBar.vertical: null

    header: Rectangle {
        height: root.listHeaderHeight
        width: parent.width
        color: "transparent"
    }

    footer: Item {
        height: root.tracksViewState.tracksVerticalScrollPadding
    }

    onVerticalScrollLockedChanged: {
        lockedVerticalScrollPosition = contentY
    }

    onContentYChanged: {
        if (verticalScrollLocked) {
            contentY = lockedVerticalScrollPosition
        } else {
            root.tracksViewState.changeTracksVerticalOffset(contentY + root.listHeaderHeight)
        }
    }

    Connections {
        target: root.tracksViewState

        function onTracksVerticalOffsetChanged() {
            root.contentY = root.tracksViewState.tracksVerticalOffset - root.listHeaderHeight
        }
    }

    function ensureVerticallyVisible(item) {
        let itemViewY = item.mapToItem(root.contentItem, Qt.point(0, 0)).y
        root.tracksViewState.ensureVerticallyVisible(contentY + root.listHeaderHeight, height, itemViewY + root.listHeaderHeight, item.height)
    }
}
