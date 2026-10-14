import QtQuick
import QtQuick.Shapes

import Muse.Ui

Item {
    id: root

    required property bool isFadeIn
    required property real fadeWidth

    //! The fade line is always drawn, the handle only when this is true
    property bool handleVisible: true

    readonly property alias containsMouse: mouseArea.containsMouse
    readonly property alias pressed: mouseArea.pressed

    signal fadeWidthChangeRequested(real width, bool completed)

    // in parent coordinates
    signal mousePositionChanged(real x, real y)

    QtObject {
        id: prv

        // The area in which the fade is drawn
        readonly property real areaWidth: root.parent ? root.parent.width : 0
        readonly property real areaHeight: root.parent ? root.parent.height : 0
    }

    // Drawn in parent coordinates: the line spans the whole fade, from the clip's edge to the fade boundary
    Shape {
        x: -root.x
        y: -root.y
        width: prv.areaWidth
        height: prv.areaHeight

        visible: root.fadeWidth > 0
        preferredRendererType: Shape.CurveRenderer

        // Linear fades for now

        // Shades the faded-out part, above the line
        ShapePath {
            strokeColor: "transparent"
            fillColor: Qt.rgba(0, 0, 0, 0.2)

            startX: root.isFadeIn ? 0 : prv.areaWidth
            startY: 0
            PathLine {
                x: root.isFadeIn ? root.fadeWidth : prv.areaWidth - root.fadeWidth
                y: 0
            }
            PathLine {
                x: root.isFadeIn ? 0 : prv.areaWidth
                y: prv.areaHeight
            }
        }

        ShapePath {
            strokeColor: Qt.rgba(0, 0, 0, 0.5)
            strokeWidth: 1
            capStyle: ShapePath.FlatCap
            fillColor: "transparent"

            startX: root.isFadeIn ? 0 : prv.areaWidth - root.fadeWidth
            startY: root.isFadeIn ? prv.areaHeight : 0
            PathLine {
                x: root.isFadeIn ? root.fadeWidth : prv.areaWidth
                y: root.isFadeIn ? 0 : prv.areaHeight
            }
        }
    }

    ClipHandleIcon {
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

        anchors.fill: parent
        visible: root.handleVisible

        hoverEnabled: true
        cursorShape: Qt.SizeHorCursor

        onPressed: function (e) {
            // in parent coordinates: the handle itself moves while dragging
            pressX = mapToItem(root.parent, e.x, e.y).x
            pressFadeWidth = root.fadeWidth
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
        }
    }
}
