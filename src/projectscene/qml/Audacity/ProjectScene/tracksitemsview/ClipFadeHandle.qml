import QtQuick

import Muse.Ui

import Audacity.ProjectScene

Item {
    id: root

    required property bool isFadeIn
    required property real fadeWidth
    //! Where the fade starts, in parent coordinates: possibly far outside the parent, which only spans the visible part of the clip
    required property real fadeLeft

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

        //! Where the fade meets the clip's edge, and where it meets the unfaded part, in parent coordinates
        readonly property real clipEdgeX: root.isFadeIn ? root.fadeLeft : root.fadeLeft + root.fadeWidth
        readonly property real boundaryX: root.isFadeIn ? root.fadeLeft + root.fadeWidth : root.fadeLeft
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

        //! From the boundary to where the handle was grabbed, so that the boundary doesn't jump to the mouse
        property real grabOffset: 0
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
            grabOffset = mapToItem(root.parent, e.x, e.y).x - prv.boundaryX
            root.fadeEditStarted()
            overrideCursor()
        }

        onCanceled: {
            root.fadeEditCanceled()
            restoreCursor()
        }

        // From the clip's edge where it is now, rather than from where the drag started: zooming or scrolling during
        // the drag moves the clip
        function requestFadeWidth(e, completed) {
            const boundaryX = mapToItem(root.parent, e.x, e.y).x - grabOffset
            root.fadeWidthChangeRequested(root.isFadeIn ? boundaryX - prv.clipEdgeX : prv.clipEdgeX - boundaryX, completed)
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
