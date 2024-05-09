import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Rectangle {

    property alias context: timelineContext

    height: 76
    color: ui.theme.backgroundPrimaryColor

    TimelineContext {
        id: timelineContext
    }

    SeparatorLine { anchors.bottom: parent.bottom }
}
