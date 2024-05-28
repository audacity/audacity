import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Rectangle {

    id: root

    property alias context: timelineContext

    color: ui.theme.backgroundPrimaryColor

    //! NOTE This element must be the same width as the track wave visible area.
    //! If this is different, than appropriate changes must be made.
    onWidthChanged: {
        timelineContext.onResizeFrameWidth(root.width)
    }

    Component.onCompleted: {
        timelineContext.init(root.width)
    }

    //! ~~~ TimelineContext ~~~
    //! NOTE See comment in TimelineContext (.h)
    function onWheel(y) {
        return timelineContext.onWheel(y)
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
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~

    TimelineRuler {
        id: ruler
        context: timelineContext
        anchors.fill: parent
    }

    SeparatorLine { anchors.bottom: parent.bottom }
}
