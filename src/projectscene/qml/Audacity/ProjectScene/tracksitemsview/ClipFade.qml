import QtQuick

import Muse.Ui

import Audacity.ProjectScene

//! A fade of a clip, with its handle. Spans the area in which the fade is drawn.
Item {
    id: root

    required property bool isFadeIn
    required property real fadeWidth
    //! Where the fade starts: possibly far outside this item, which only spans the visible part of the clip
    required property real fadeX
    //! Where the center of the boundary line is: possibly outside the fade
    required property real lineX
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
    signal fadeTypeChangeRequested(int type)
    signal fadeLengthEditRequested
    signal fadeRemoveRequested

    // in this item's coordinates
    signal mousePositionChanged(real x, real y)

    QtObject {
        id: prv

        readonly property real lineHalfWidth: 0.5

        // Of what is drawn, the fade and its line, which may stick out of it, what is visible
        readonly property real visibleLeft: {
            const drawnLeft = Math.min(root.fadeX, root.lineX - lineHalfWidth)
            return Math.max(drawnLeft, 0)
        }

        readonly property real visibleRight: {
            const drawnRight = Math.max(root.fadeX + root.fadeWidth, root.lineX + lineHalfWidth)
            return Math.min(drawnRight, root.width)
        }
    }

    // Only spans the visible part of the fade and its line: when zoomed in, the whole of the fade could be huge.
    // Rounded outwards, so that the item's size, which Qt rounds, doesn't crop the fade's ends.
    ClipFadeCurveItem {
        id: curve

        x: Math.floor(prv.visibleLeft)
        width: Math.max(0, Math.ceil(prv.visibleRight) - x)
        height: root.height
        visible: width > 0

        isFadeIn: root.isFadeIn
        fadeX: root.fadeX - x
        fadeWidth: root.fadeWidth
        lineX: root.lineX - x
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
        fadeX: root.fadeX
        fadeType: root.fadeType

        onFadeEditStarted: root.fadeEditStarted()

        onFadeWidthChangeRequested: function (width, completed) {
            root.fadeWidthChangeRequested(width, completed)
        }

        onFadeEditCanceled: root.fadeEditCanceled()

        onFadeTypeChangeRequested: function (type) {
            root.fadeTypeChangeRequested(type)
        }

        onFadeLengthEditRequested: root.fadeLengthEditRequested()
        onFadeRemoveRequested: root.fadeRemoveRequested()

        onMousePositionChanged: function (mouseX, mouseY) {
            root.mousePositionChanged(mouseX, mouseY)
        }
    }
}
