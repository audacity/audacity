import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Item {
    id: root

    required property bool isFadeIn
    required property real fadeWidth
    //! Where the fade starts, in parent coordinates: possibly far outside the parent, which only spans the visible part of the clip
    required property real fadeX
    required property int fadeType

    //! The fade line is always drawn, the handle only when this is true
    property bool handleVisible: true

    readonly property alias pressed: mouseArea.dragging

    signal fadeEditStarted
    signal fadeWidthChangeRequested(real width, bool completed)
    signal fadeEditCanceled
    signal fadeTypeChangeRequested(int type)

    // in parent coordinates
    signal mousePositionChanged(real x, real y)

    QtObject {
        id: prv

        //! Where the fade meets the clip's edge, and where it meets the unfaded part, in parent coordinates
        readonly property real clipEdgeX: root.isFadeIn ? root.fadeX : root.fadeX + root.fadeWidth
        readonly property real boundaryX: root.isFadeIn ? root.fadeX + root.fadeWidth : root.fadeX

        readonly property var fadeTypes: [
            {
                type: FadeType.LINEAR,
                title: qsTrc("projectscene", "Linear")
            },
            {
                type: FadeType.EXPONENTIAL,
                title: qsTrc("projectscene", "Exponential")
            },
            {
                type: FadeType.LOGARITHMIC,
                title: qsTrc("projectscene", "Logarithmic")
            }
        ]

        function openContextMenu(x, y) {
            contextMenuLoader.show(Qt.point(x, y), fadeTypes.map(fadeType => ({
                        id: String(fadeType.type),
                        title: fadeType.title,
                        checkable: true,
                        checked: fadeType.type === root.fadeType
                    })))
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

        hovered: mouseArea.containsMouse || mouseArea.dragging
        iconCode: root.isFadeIn ? IconCode.FADE_HANDLE_IN : IconCode.FADE_HANDLE_OUT
        color: ui.theme.extra["black_color"]
        style: Text.Outline
        styleColor: ui.theme.extra["white_color"]
    }

    ContextMenuLoader {
        id: contextMenuLoader

        onHandleMenuItem: function (itemId) {
            const type = parseInt(itemId)
            if (type !== root.fadeType) {
                root.fadeTypeChangeRequested(type)
            }
        }
    }

    MouseArea {
        id: mouseArea

        //! Only the left button drags the handle, the right one opens the context menu
        property bool dragging: false

        //! From the boundary to where the handle was grabbed, so that the boundary doesn't jump to the mouse
        property real grabOffset: 0
        property bool cursorOverridden: false

        // Active whether the handle is shown or not. Over it, a shown handle grows; a hidden one turns the mouse
        // cursor into the hovered handle.
        anchors.fill: parent

        acceptedButtons: Qt.LeftButton | Qt.RightButton
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
            // The context menu opens on release, like the clips'
            if (e.button === Qt.RightButton) {
                return
            }
            dragging = true
            // in parent coordinates: the handle itself moves while dragging
            grabOffset = mapToItem(root.parent, e.x, e.y).x - prv.boundaryX
            root.fadeEditStarted()
            overrideCursor()
        }

        onCanceled: {
            if (!dragging) {
                return
            }
            dragging = false
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

            if (dragging) {
                requestFadeWidth(e, false)
            }
        }

        onReleased: function (e) {
            if (e.button === Qt.RightButton) {
                if (!dragging && containsMouse) {
                    prv.openContextMenu(e.x, e.y)
                }
                return
            }
            if (!dragging) {
                return
            }
            dragging = false
            requestFadeWidth(e, true)
            restoreCursor()
        }
    }
}
