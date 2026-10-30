import QtQuick

import Muse.Ui

import Audacity.ProjectScene

//! A fade of a clip, with its handle. Spans the area in which the fade is drawn.
Item {
    id: root

    required property bool isFadeIn
    required property real fadeWidth
    //! Where the fade starts: possibly far outside this item, which only spans the visible part of the clip
    required property real fadeLeft
    required property int fadeType
    //! Above it, the fade isn't drawn
    property real drawingTop: 0

    property alias handleX: handle.x
    property alias handleY: handle.y
    property real handleSize: 32
    property alias handleVisible: handle.handleVisible

    readonly property alias pressed: handle.pressed

    signal fadeEditStarted
    signal fadeWidthChangeRequested(real width, bool completed)
    signal fadeEditCanceled

    // in this item's coordinates
    signal mousePositionChanged(real x, real y)

    // Only spans the visible part of the fade: when zoomed in, the whole of it could be huge. Rounded outwards, so
    // that the item's size, which Qt rounds, doesn't crop the fade's ends.
    ClipFadeCurveItem {
        id: curve

        x: Math.floor(Math.max(root.fadeLeft, 0))
        width: Math.max(0, Math.ceil(Math.min(root.fadeLeft + root.fadeWidth, root.width)) - x)
        height: root.height
        visible: width > 0

        isFadeIn: root.isFadeIn
        fadeLeft: root.fadeLeft - x
        fadeWidth: root.fadeWidth
        fadeType: root.fadeType
        drawingTop: root.drawingTop
        highlighted: handle.pressed
    }

    ClipFadeHandle {
        id: handle

        width: root.handleSize
        height: root.handleSize

        isFadeIn: root.isFadeIn
        fadeWidth: root.fadeWidth
        fadeLeft: root.fadeLeft

        onFadeEditStarted: root.fadeEditStarted()

        onFadeWidthChangeRequested: function (width, completed) {
            root.fadeWidthChangeRequested(width, completed)
        }

        onFadeEditCanceled: root.fadeEditCanceled()

        onMousePositionChanged: function (mouseX, mouseY) {
            root.mousePositionChanged(mouseX, mouseY)
        }
    }
}
