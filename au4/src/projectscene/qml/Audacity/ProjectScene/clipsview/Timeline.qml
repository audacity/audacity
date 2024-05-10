import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Rectangle {

    id: root

    property alias context: timelineContext

    height: 76
    color: ui.theme.backgroundPrimaryColor

    function onWheel(y) {
        timelineContext.onWheel(y)
    }

    function onSelection(x1, x2) {
        timelineContext.onSelection(x1, x2)
    }

    function resetSelection() {
        timelineContext.resetSelection()
    }

    TimelineContext {
        id: timelineContext
    }

    StyledTextLabel {
        anchors.fill: parent
        anchors.leftMargin: 16
        text: "zoom: " + timelineContext.zoom + ", offset: " + timelineContext.offset
    }

    SeparatorLine { anchors.bottom: parent.bottom }
}
