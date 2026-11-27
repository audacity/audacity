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

    property bool clipSelected: false
    property bool clipHovered: false

    //! The fades span the whole item, the handles only a band of it, starting at `handlesY`
    property real handlesY: 0
    //! Above it, the fades aren't drawn
    property real drawingTop: 0

    signal fadeEditStarted(int side)
    signal fadeWidthChangeRequested(real width, bool completed)
    signal fadeEditCanceled
    signal fadeInTypeChangeRequested(int type)
    signal fadeOutTypeChangeRequested(int type)
    signal fadeInLengthEditRequested
    signal fadeOutLengthEditRequested
    signal fadeInRemoveRequested
    signal fadeOutRemoveRequested

    // mouse position event is not propagated on overlapping mouse areas
    // so we are handling it manually
    signal fadeHandlesMousePositionChanged(real x, real y)

    QtObject {
        id: prv

        readonly property int handleSize: 32

        readonly property real fadeInX: -root.clipOffset
        readonly property real fadeOutX: root.clipWidth - root.clipOffset - root.fadeOutWidth

        readonly property real lineHalfWidth: 0.5
        // Each line is centered on where its fade ends: so, where the fades meet, their lines are one. Though, to be
        // seen whole, not further out than the clip's first and last half pixels.
        readonly property real fadeInLineCenter: {
            const clipEndX = root.clipWidth - root.clipOffset
            return Math.min(fadeInX + root.fadeInWidth, clipEndX - lineHalfWidth)
        }
        readonly property real fadeOutLineCenter: Math.max(fadeOutX, fadeInX + lineHalfWidth)

        // When the handles collide, each is pushed to its side by half the overlap: when the lines are aligned,
        // the handles are on either side of them.
        readonly property real collisionPush: Math.max(0, handleSize - (fadeOutLineCenter - fadeInLineCenter)) / 2

        //! Centered on the boundary line, but inside the clip
        function handleX(lineCenter) {
            return Math.max(0, Math.min(lineCenter - handleSize / 2, root.width - handleSize))
        }

        // Where the handles overlap, the dragged one is on top, else the longer fade's: e.g., with no fade-in and
        // a fade-out spanning the whole clip, both handles are in the left corner, and only the fade-out is hittable.
        readonly property bool fadeInOnTop: fadeIn.pressed || (!fadeOut.pressed && root.fadeInWidth > root.fadeOutWidth)
    }

    ClipFade {
        id: fadeIn

        anchors.fill: parent
        z: prv.fadeInOnTop ? 1 : 0

        isFadeIn: true
        fadeWidth: root.fadeInWidth
        fadeX: prv.fadeInX
        lineX: prv.fadeInLineCenter
        fadeType: root.fadeInType
        drawingTop: root.drawingTop

        handleX: prv.handleX(prv.fadeInLineCenter - prv.collisionPush)
        handleY: root.handlesY
        handleSize: prv.handleSize
        handleVisible: root.clipSelected || (root.clipHovered && root.fadeInWidth > 0)

        onFadeEditStarted: root.fadeEditStarted(ClipSide.Left)

        onFadeWidthChangeRequested: function (width, completed) {
            root.fadeWidthChangeRequested(width, completed)
        }

        onFadeEditCanceled: root.fadeEditCanceled()

        onFadeTypeChangeRequested: function (type) {
            root.fadeInTypeChangeRequested(type)
        }

        onFadeLengthEditRequested: root.fadeInLengthEditRequested()
        onFadeRemoveRequested: root.fadeInRemoveRequested()

        onMousePositionChanged: function (mouseX, mouseY) {
            root.fadeHandlesMousePositionChanged(mouseX, mouseY)
        }
    }

    ClipFade {
        id: fadeOut

        anchors.fill: parent
        z: prv.fadeInOnTop ? 0 : 1

        isFadeIn: false
        fadeWidth: root.fadeOutWidth
        fadeX: prv.fadeOutX
        lineX: prv.fadeOutLineCenter
        fadeType: root.fadeOutType
        drawingTop: root.drawingTop

        handleX: prv.handleX(prv.fadeOutLineCenter + prv.collisionPush)
        handleY: root.handlesY
        handleSize: prv.handleSize
        handleVisible: root.clipSelected || (root.clipHovered && root.fadeOutWidth > 0)

        onFadeEditStarted: root.fadeEditStarted(ClipSide.Right)

        onFadeWidthChangeRequested: function (width, completed) {
            root.fadeWidthChangeRequested(width, completed)
        }

        onFadeEditCanceled: root.fadeEditCanceled()

        onFadeTypeChangeRequested: function (type) {
            root.fadeOutTypeChangeRequested(type)
        }

        onFadeLengthEditRequested: root.fadeOutLengthEditRequested()
        onFadeRemoveRequested: root.fadeOutRemoveRequested()

        onMousePositionChanged: function (mouseX, mouseY) {
            root.fadeHandlesMousePositionChanged(mouseX, mouseY)
        }
    }
}
