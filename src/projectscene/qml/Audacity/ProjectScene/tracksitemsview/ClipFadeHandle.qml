import QtQuick
import QtQuick.Shapes

import Muse.Ui

import Audacity.ProjectScene

Item {
    id: root

    required property bool isFadeIn
    required property real fadeWidth
    //! Where the fade starts, in parent coordinates: possibly far outside the parent, which only spans the visible part of the clip
    required property real fadeLeft
    required property int fadeType

    //! The fade line is always drawn, the handle only when this is true
    property bool handleVisible: true

    readonly property alias pressed: mouseArea.pressed

    signal fadeEditStarted
    signal fadeWidthChangeRequested(real width, bool completed)
    signal fadeEditCanceled

    // in parent coordinates
    signal mousePositionChanged(real x, real y)

    QtObject {
        id: prv

        // The area in which the fade is drawn
        readonly property real areaWidth: root.parent ? root.parent.width : 0
        readonly property real areaHeight: root.parent ? root.parent.height : 0

        //! The part of the fade within the area: when zoomed in, drawing all of it would mean huge coordinates
        readonly property real visibleLeft: Math.max(root.fadeLeft, 0)
        readonly property real visibleRight: Math.min(root.fadeLeft + root.fadeWidth, areaWidth)

        readonly property var linePoints: {
            const width = root.fadeWidth
            const visibleWidth = visibleRight - visibleLeft
            if (width <= 0 || visibleWidth <= 0) {
                return []
            }
            // PathPolyline will linearly interpolate between datapoints, so we can afford some
            // downsampling here:
            const segmentCount = Math.max(1, Math.ceil(visibleWidth / 2))
            const positions = []
            for (let i = 0; i <= segmentCount; ++i) {
                positions.push((visibleLeft + i / segmentCount * visibleWidth - root.fadeLeft) / width)
            }
            // From the silent end of the fade
            const gains = ClipFadeGains.gainsAt(root.fadeType, root.isFadeIn ? positions : positions.map(p => 1 - p))
            return positions.map((p, i) => Qt.point(root.fadeLeft + p * width, areaHeight * (1 - gains[i])))
        }

        //! The faded-out part, above the line: the line closed by the top of the area
        readonly property var shadingPoints: linePoints.length ? [Qt.point(visibleLeft, 0)].concat(linePoints).concat([Qt.point(visibleRight, 0)]) : []
    }

    // Drawn in parent coordinates: the line spans the whole fade, from the clip's edge to the fade boundary
    Shape {
        x: -root.x
        y: -root.y
        width: prv.areaWidth
        height: prv.areaHeight

        visible: prv.linePoints.length > 0
        preferredRendererType: Shape.CurveRenderer

        // Shades the faded-out part, above the line
        ShapePath {
            strokeColor: "transparent"
            fillColor: Qt.rgba(0, 0, 0, 0.2)

            PathPolyline {
                path: prv.shadingPoints
            }
        }

        ShapePath {
            strokeColor: Qt.rgba(0, 0, 0, 0.5)
            strokeWidth: 1
            capStyle: ShapePath.FlatCap
            fillColor: "transparent"

            PathPolyline {
                path: prv.linePoints
            }
        }

        // Highlights the line while the handle is dragged
        ShapePath {
            strokeColor: mouseArea.pressed ? ui.theme.extra["white_color"] : "transparent"
            strokeWidth: 1.5
            capStyle: ShapePath.FlatCap
            fillColor: "transparent"

            PathPolyline {
                path: prv.linePoints
            }
        }
    }

    ClipHandleIcon {
        id: icon

        anchors.centerIn: parent

        opacity: root.handleVisible ? 1 : 0
        visible: opacity > 0

        Behavior on opacity {
            NumberAnimation {
                duration: 80
            }
        }

        hovered: mouseArea.containsMouse || mouseArea.pressed
        iconCode: root.isFadeIn ? IconCode.FADE_HANDLE_IN : IconCode.FADE_HANDLE_OUT
        color: ui.theme.extra["black_color"]
        style: Text.Outline
        styleColor: ui.theme.extra["white_color"]
    }

    MouseArea {
        id: mouseArea

        property real pressX: 0
        property real pressFadeWidth: 0
        property bool cursorOverridden: false

        // Active whether the handle is shown or not. Over it, a shown handle grows; a hidden one turns the mouse
        // cursor into the hovered handle.
        anchors.fill: parent

        hoverEnabled: true

        function updateCursor() {
            if (root.handleVisible) {
                CustomCursorProvider.setStandardCursorShape(mouseArea, Qt.SizeHorCursor)
            } else {
                CustomCursorProvider.setIconCursorShape(mouseArea, ui.theme.iconsFont.family, icon.iconCode, icon.hoveredIconSize)
            }
        }

        Component.onCompleted: updateCursor()

        Connections {
            target: root
            function onHandleVisibleChanged() {
                mouseArea.updateCursor()
            }
        }

        Component.onDestruction: restoreCursor()

        // While dragging, the mouse may go over the other handle, which would show its own cursor
        function overrideCursor() {
            if (root.handleVisible) {
                CustomCursorProvider.overrideStandardCursor(Qt.SizeHorCursor)
            } else {
                CustomCursorProvider.overrideIconCursor(ui.theme.iconsFont.family, icon.iconCode, icon.hoveredIconSize)
            }
            cursorOverridden = true
        }

        function restoreCursor() {
            if (cursorOverridden) {
                CustomCursorProvider.restoreCursor()
                cursorOverridden = false
            }
        }

        onPressed: function (e) {
            // in parent coordinates: the handle itself moves while dragging
            pressX = mapToItem(root.parent, e.x, e.y).x
            pressFadeWidth = root.fadeWidth
            root.fadeEditStarted()
            overrideCursor()
        }

        onCanceled: {
            root.fadeEditCanceled()
            restoreCursor()
        }

        function requestFadeWidth(e, completed) {
            const dx = mapToItem(root.parent, e.x, e.y).x - pressX
            root.fadeWidthChangeRequested(pressFadeWidth + (root.isFadeIn ? dx : -dx), completed)
        }

        onPositionChanged: function (e) {
            const pos = mapToItem(root.parent, e.x, e.y)
            root.mousePositionChanged(pos.x, pos.y)

            if (pressed) {
                requestFadeWidth(e, false)
            }
        }

        onReleased: function (e) {
            requestFadeWidth(e, true)
            restoreCursor()
        }
    }
}
