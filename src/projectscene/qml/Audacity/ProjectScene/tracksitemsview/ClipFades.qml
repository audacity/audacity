import QtQuick

import Audacity.ProjectScene

Item {
    id: root

    // Don't draw on the clip header
    clip: true

    property real fadeInWidth: 0
    property real fadeOutWidth: 0

    //! This item may only span part of the clip: how much of the clip is on its left, and the clip's whole width
    property real clipOffset: 0
    property real clipWidth: width
    required property int fadeInType
    required property int fadeOutType

    //! The fades span the whole item, the handles only a band of it, starting at `handlesY`
    property real handlesY: 0
    //! Above it, the fades aren't drawn
    property real drawingTop: 0
    property bool handlesVisible: false

    signal fadeEditStarted
    signal fadeInWidthChangeRequested(real width, bool completed)
    signal fadeOutWidthChangeRequested(real width, bool completed)
    signal fadeEditCanceled

    // mouse position event is not propagated on overlapping mouse areas
    // so we are handling it manually
    signal fadeHandlesMousePositionChanged(real x, real y)

    QtObject {
        id: prv

        readonly property int handleSize: 32

        //! Where the fades end and start, in this item's coordinates
        readonly property real fadeInEnd: root.fadeInWidth - root.clipOffset
        readonly property real fadeOutStart: root.clipWidth - root.clipOffset - root.fadeOutWidth

        readonly property real fadeInLineCenter: fadeInEnd - 0.5
        readonly property real fadeOutLineCenter: fadeOutStart + 0.5

        // When the handles collide, each is pushed to its side by half the overlap: when the lines are aligned,
        // the handles are on either side of them.
        readonly property real collisionPush: Math.max(0, handleSize - (fadeOutLineCenter - fadeInLineCenter)) / 2

        //! Centered on the boundary line, but inside the clip
        function handleX(lineCenter) {
            return Math.max(0, Math.min(lineCenter - handleSize / 2, root.width - handleSize))
        }

        // Where the handles overlap, the dragged one is on top, else the longer fade's: e.g., with no fade-in and
        // a fade-out spanning the whole clip, both handles are in the left corner, and only the fade-out is hittable.
        readonly property bool fadeInOnTop: fadeInHandle.pressed || (!fadeOutHandle.pressed && root.fadeInWidth > root.fadeOutWidth)
    }

    // the boundary lines are drawn just inside their fade
    ClipFadeBoundaryLine {
        y: root.drawingTop
        height: root.height - root.drawingTop
        x: prv.fadeInEnd - 1
        visible: root.fadeInWidth >= 1
        highlighted: fadeInHandle.pressed
    }

    ClipFadeBoundaryLine {
        y: root.drawingTop
        height: root.height - root.drawingTop
        x: prv.fadeOutStart
        visible: root.fadeOutWidth >= 1
        highlighted: fadeOutHandle.pressed
    }

    ClipFadeHandle {
        id: fadeInHandle

        x: prv.handleX(prv.fadeInLineCenter - prv.collisionPush)
        y: root.handlesY
        z: prv.fadeInOnTop ? 1 : 0
        width: prv.handleSize
        height: prv.handleSize
        handleVisible: root.handlesVisible

        isFadeIn: true
        fadeWidth: root.fadeInWidth
        fadeLeft: prv.fadeInEnd - root.fadeInWidth
        fadeType: root.fadeInType
        drawingTop: root.drawingTop

        onFadeEditStarted: root.fadeEditStarted()

        onFadeWidthChangeRequested: function (width, completed) {
            root.fadeInWidthChangeRequested(width, completed)
        }

        onFadeEditCanceled: root.fadeEditCanceled()

        onMousePositionChanged: function (mouseX, mouseY) {
            root.fadeHandlesMousePositionChanged(mouseX, mouseY)
        }
    }

    ClipFadeHandle {
        id: fadeOutHandle

        x: prv.handleX(prv.fadeOutLineCenter + prv.collisionPush)
        y: root.handlesY
        z: prv.fadeInOnTop ? 0 : 1
        width: prv.handleSize
        height: prv.handleSize
        handleVisible: root.handlesVisible

        isFadeIn: false
        fadeWidth: root.fadeOutWidth
        fadeLeft: prv.fadeOutStart
        fadeType: root.fadeOutType
        drawingTop: root.drawingTop

        onFadeEditStarted: root.fadeEditStarted()

        onFadeWidthChangeRequested: function (width, completed) {
            root.fadeOutWidthChangeRequested(width, completed)
        }

        onFadeEditCanceled: root.fadeEditCanceled()

        onMousePositionChanged: function (mouseX, mouseY) {
            root.fadeHandlesMousePositionChanged(mouseX, mouseY)
        }
    }
}
