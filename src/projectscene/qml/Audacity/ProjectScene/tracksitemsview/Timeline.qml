import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Rectangle {
    id: root

    property alias context: timelineContext
    property alias ruler: timelineRuler
    property NavigationSection navigationSection: null

    color: ui.theme.backgroundSecondaryColor

    //! NOTE This element must be the same width as the track wave visible area.
    //! If this is different, then appropriate changes must be made.
    onWidthChanged: {
        if (root.visible) {
            timelineContext.onResizeFrameWidth(root.width)
        }
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

    function isMajorSection(y) {
        return timelineRuler.isMajorSection(y)
    }

    NavigationPanel {
        id: navPanel
        name: "TimelinePanel"
        enabled: root.enabled && root.visible
        section: root.navigationSection
        direction: NavigationPanel.Horizontal
        order: 0

        accessible.name: qsTrc("projectscene", "Timeline")
    }

    NavigationControl {
        id: navCtrl
        name: "Timeline"
        enabled: root.enabled && root.visible
        panel: navPanel
        order: 0

        accessible.role: MUAccessible.Information
        accessible.name: qsTrc("projectscene", "Timeline")

        onActiveChanged: function (active) {
            if (active) {
                root.forceActiveFocus()
            }
        }
    }

    TimelineContextMenuModel {
        id: contextMenuModel
    }

    ContextMenuLoader {
        id: contextMenuLoader

        onHandleMenuItem: function (itemId) {
            contextMenuModel.handleMenuItem(itemId)
        }
    }

    TimelineContext {
        id: timelineContext
    }
    // ~~~~~~~~~~~~~~~~~~~~~~~~~~

    PlayRegion {
        id: playRegion

        context: timelineContext
    }

    TimelineRuler {
        id: timelineRuler

        anchors.fill: parent

        context: timelineContext
    }

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.RightButton
        // MouseArea defaults cursorShape to ArrowCursor even with acceptedButtons: Qt.RightButton.
        // Setting undefined lets the underlying PlayRegion cursors through.
        cursorShape: undefined
        onClicked: function (e) {
            contextMenuModel.load()
            contextMenuLoader.show(Qt.point(e.x, e.y), contextMenuModel.items)
        }
    }

    SeparatorLine {
        anchors.bottom: parent.bottom
    }

    NavigationFocusBorder {
        navigationCtrl: navCtrl
        drawOutsideParent: false
    }
}
