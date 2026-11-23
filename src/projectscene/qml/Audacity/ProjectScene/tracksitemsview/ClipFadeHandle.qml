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

    //! The fade line is always drawn, the handle only when this is true or when its hit area is hovered
    property bool handleVisible: true

    readonly property alias pressed: mouseArea.dragging

    signal fadeEditStarted
    signal fadeWidthChangeRequested(real width, bool completed)
    signal fadeEditCanceled
    signal fadeTypeChangeRequested(int type)
    signal fadeLengthEditRequested
    signal fadeRemoveRequested

    // in parent coordinates
    signal mousePositionChanged(real x, real y)

    QtObject {
        id: prv

        //! Where the fade meets the clip's edge, and where it meets the unfaded part, in parent coordinates
        readonly property real clipEdgeX: root.isFadeIn ? root.fadeX : root.fadeX + root.fadeWidth
        readonly property real boundaryX: root.isFadeIn ? root.fadeX + root.fadeWidth : root.fadeX

        readonly property string editLengthItemId: "editLength"
        readonly property string removeItemId: "remove"

        function openContextMenu(x, y) {
            const typeItems = FadeTypeOptions.all.map(fadeType => ({
                        id: String(fadeType.value),
                        title: fadeType.text,
                        checkable: true,
                        checked: fadeType.value === root.fadeType
                    }))
            const editLengthItem = {
                id: editLengthItemId,
                title: root.isFadeIn ? qsTrc("projectscene", "Edit fade-in length…") : qsTrc("projectscene", "Edit fade-out length…")
            }
            const removeItem = {
                id: removeItemId,
                title: root.isFadeIn ? qsTrc("projectscene", "Remove fade-in") : qsTrc("projectscene", "Remove fade-out")
            }
            const menuSeparator = {}
            contextMenuLoader.show(Qt.point(x, y), typeItems.concat([menuSeparator, editLengthItem, removeItem]))
        }
    }

    ClipHandleIcon {
        id: icon

        anchors.centerIn: parent

        opacity: root.handleVisible || mouseArea.containsMouse || mouseArea.dragging ? 1 : 0
        visible: opacity > 0

        hovered: mouseArea.containsMouse || mouseArea.dragging
        iconCode: root.isFadeIn ? IconCode.FADE_HANDLE_IN : IconCode.FADE_HANDLE_OUT
        color: ui.theme.extra["black_color"]
        style: Text.Outline
        styleColor: ui.theme.extra["white_color"]
    }

    ContextMenuLoader {
        id: contextMenuLoader

        onHandleMenuItem: function (itemId) {
            if (itemId === prv.editLengthItemId) {
                root.fadeLengthEditRequested()
                return
            }
            if (itemId === prv.removeItemId) {
                root.fadeRemoveRequested()
                return
            }
            // Even the type this fade has: the other selected clips may have another
            root.fadeTypeChangeRequested(parseInt(itemId))
        }
    }

    MouseArea {
        id: mouseArea

        //! Only the left button drags the handle, the right one opens the context menu
        property bool dragging: false

        //! From the boundary to where the handle was grabbed, so that the boundary doesn't jump to the mouse
        property real grabOffset: 0
        //! Until it moves from where it was pressed, the mouse changes nothing: e.g. snapping would otherwise move an
        //! off-grid fade on a mere click
        property real pressX: 0
        property bool moved: false
        property bool cursorOverridden: false

        anchors.fill: parent

        acceptedButtons: Qt.LeftButton | Qt.RightButton
        hoverEnabled: true

        Component.onCompleted: CustomCursorProvider.setStandardCursorShape(mouseArea, Qt.SizeHorCursor)

        Component.onDestruction: restoreCursor()

        // While dragging, the mouse may go over other items, which would show their own cursors
        function overrideCursor() {
            CustomCursorProvider.overrideStandardCursor(Qt.SizeHorCursor)
            cursorOverridden = true
        }

        function restoreCursor() {
            if (cursorOverridden) {
                CustomCursorProvider.restoreCursor()
                cursorOverridden = false
            }
        }

        onPressed: function (e) {
            // The context menu opens on release, like the clips', and only where there is a fade: elsewhere, the
            // press goes to the items below
            if (e.button === Qt.RightButton) {
                e.accepted = root.fadeWidth > 0
                return
            }
            dragging = true
            moved = false
            // in parent coordinates: the handle itself moves while dragging
            pressX = mapToItem(root.parent, e.x, e.y).x
            grabOffset = pressX - prv.boundaryX
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
                moved = moved || pos.x !== pressX
                if (moved) {
                    requestFadeWidth(e, false)
                }
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
            if (moved) {
                requestFadeWidth(e, true)
            } else {
                root.fadeEditCanceled()
            }
            restoreCursor()
        }

        onDoubleClicked: function (e) {
            if (e.button !== Qt.LeftButton) {
                return
            }
            // The second press started a drag, whose release would otherwise set the fade back
            if (dragging) {
                dragging = false
                root.fadeEditCanceled()
                restoreCursor()
            }
            if (root.fadeWidth > 0) {
                root.fadeRemoveRequested()
            }
        }
    }
}
