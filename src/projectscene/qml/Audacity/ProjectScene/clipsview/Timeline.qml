import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Rectangle {

    id: root

    property alias context: timelineContext
    property alias ruler: timelineRuler

    signal clicked(var mouse)

    color: ui.theme.backgroundSecondaryColor

    //! NOTE This element must be the same width as the track wave visible area.
    //! If this is different, than appropriate changes must be made.
    onWidthChanged: {
        timelineContext.onResizeFrameWidth(root.width)
    }

    function init() {
        timelineContext.init(root.width)
    }

    //! ~~~ TimelineContext ~~~
    //! NOTE See comment in TimelineContext (.h)
    function onWheel(mouseX, pixelDelta, angleDelta) {
        return timelineContext.onWheel(mouseX, pixelDelta, angleDelta)
    }

    function onSelection(x1, x2) {
        timelineContext.onSelection(x1, x2)
    }

    function resetSelection() {
        timelineContext.resetSelection()
    }

    TimelineContextMenuModel {
        id: contextMenuModel
    }

    ContextMenuLoader {
        id: contextMenuLoader

        onHandleMenuItem: function(itemId) {
            contextMenuModel.handleMenuItem(itemId)
        }
    }

    TimelineContext {
        id: timelineContext
    }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~

    TimelineRuler {
        id: timelineRuler
        anchors.fill: parent
        context: timelineContext
    }

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        onClicked: e => {
                       if (e.button === Qt.LeftButton) {
                           root.clicked(e)
                       } else if (e.button === Qt.RightButton) {
                           contextMenuModel.load()
                           contextMenuLoader.show(Qt.point(e.x, e.y), contextMenuModel.items)
                       }
                   }
    }

    SeparatorLine { anchors.bottom: parent.bottom }
}
