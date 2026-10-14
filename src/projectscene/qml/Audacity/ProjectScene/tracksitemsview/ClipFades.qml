import QtQuick

Item {
    id: root

    // Don't draw on the clip header
    clip: true

    property real fadeInWidth: 0
    property real fadeOutWidth: 0

    //! The fades span the whole item, the handles only a band of it, starting at `handlesY`
    property real handlesY: 0
    property int handleHeight: 32
    property bool handlesVisible: false

    readonly property bool hovered: fadeInHandle.containsMouse || fadeOutHandle.containsMouse
    readonly property bool dragged: fadeInHandle.pressed || fadeOutHandle.pressed

    signal fadeInWidthChangeRequested(real width, bool completed)
    signal fadeOutWidthChangeRequested(real width, bool completed)

    // mouse position event is not propagated on overlapping mouse areas
    // so we are handling it manually
    signal fadeHandlesMousePositionChanged(real x, real y)

    QtObject {
        id: prv

        readonly property int handleWidth: 30

        // While dragging, the cursor may leave the handle (and the clip), and the clip doesn't
        // get hover updates during the drag: keep both handles up until release.
        readonly property bool handlesShown: root.handlesVisible || root.dragged

        function clampX(x) {
            return Math.max(0, Math.min(x, root.width - handleWidth))
        }
    }

    // the boundary lines are drawn just inside their fade
    ClipFadeBoundaryLine {
        height: root.height
        x: root.fadeInWidth - 1
        visible: root.fadeInWidth >= 1
    }

    ClipFadeBoundaryLine {
        height: root.height
        x: root.width - root.fadeOutWidth
        visible: root.fadeOutWidth >= 1
    }

    ClipFadeHandle {
        id: fadeInHandle

        y: root.handlesY
        width: prv.handleWidth
        height: root.handleHeight
        handleVisible: prv.handlesShown

        isFadeIn: true
        fadeWidth: root.fadeInWidth

        // sits outside its fade, against the fade boundary
        x: prv.clampX(root.fadeInWidth)

        onFadeWidthChangeRequested: function (width, completed) {
            root.fadeInWidthChangeRequested(width, completed)
        }

        onMousePositionChanged: function (mouseX, mouseY) {
            root.fadeHandlesMousePositionChanged(mouseX, mouseY)
        }
    }

    ClipFadeHandle {
        id: fadeOutHandle

        y: root.handlesY
        width: prv.handleWidth
        height: root.handleHeight
        handleVisible: prv.handlesShown

        isFadeIn: false
        fadeWidth: root.fadeOutWidth

        // sits outside its fade, against the fade boundary
        x: prv.clampX(root.width - root.fadeOutWidth - prv.handleWidth)

        onFadeWidthChangeRequested: function (width, completed) {
            root.fadeOutWidthChangeRequested(width, completed)
        }

        onMousePositionChanged: function (mouseX, mouseY) {
            root.fadeHandlesMousePositionChanged(mouseX, mouseY)
        }
    }
}
