/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

ListItemBlank {
    id: root

    property var item: null
    property var container: null
    property bool dragged: false
    property bool collapsed: trackViewState.isTrackCollapsed

    property bool isFocused: false

    property alias headerTrailingControlsComponent: headerTrailingControls.sourceComponent
    property alias extraControlsComponent: extraControlsLoader.sourceComponent
    property alias rightSideContainerComponent: rightSideContainer.sourceComponent

    property alias bottomSeparatorHeight: bottomSeparator.height

    signal interactionStarted()
    signal interactionEnded()
    signal selectionRequested(bool exclusive)

    signal mousePressed(var item, double x, double y)
    signal mouseReleased(var item, double x, double y)
    signal mouseMoved(var item, double x, double y)

    mouseArea.onPressed: {
        root.mousePressed(this, mouseArea.mouseX, mouseArea.mouseY)
    }

    mouseArea.onReleased: {
        root.mouseReleased(this, mouseArea.mouseX, mouseArea.mouseY)
    }

    mouseArea.onPositionChanged: {
        root.mouseMoved(this, mouseArea.mouseX, mouseArea.mouseY)
    }

    mouseArea.onDoubleClicked: {
        let titlePos = root.mapFromItem(title, 0, 0)
        if (mouseArea.mouseX >= titlePos.x && mouseArea.mouseX <= titlePos.x + title.width &&
            mouseArea.mouseY >= titlePos.y && mouseArea.mouseY <= titlePos.y + title.height) {
            title.edit()
        }
    }

    height: trackViewState.trackHeight
    opacity: dragged ? 0.5 : 1

    focusBorder.anchors.leftMargin: spacer.width + 2
    focusBorder.anchors.rightMargin: 24 + separatorLine.width
    focusBorder.anchors.bottomMargin: 2

    background.color: (root.isSelected || hoverHandler.hovered) ?
                   ui.theme.backgroundPrimaryColor : ui.theme.backgroundSecondaryColor

    background.opacity: (!root.isSelected || hoverHandler.hovered) ? 0.7 : 1

    signal renameTrackRequested()
    signal duplicateRequested()
    signal deleteRequested()

    signal openEffectsRequested()

    property TrackViewStateModel trackViewState: TrackViewStateModel {
        trackId: root.item ? root.item.trackId : -1
    }

    TrackContextMenuModel {
        id: contextMenuModel
        trackId: root.item ? root.item.trackId : -1

        onTrackRenameRequested: title.edit()
    }

    function init() {
        trackViewState.init()
        contextMenuModel.load()
        // Disable ancestor's states
        // to provide custom styling
        states = []
    }

    ContextMenuLoader {
        id: contextMenuLoader

        onHandleMenuItem: function(itemId) {
            contextMenuModel.handleMenuItem(itemId)
        }
    }

    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.RightButton

        onClicked: function(e) {
            if (!isSelected) {
                root.selectionRequested(true)
            }

            contextMenuLoader.show(Qt.point(e.x, e.y), contextMenuModel.items)
        }
    }

    Item {
        anchors.fill: parent

        Rectangle {
            id: spacer

            anchors.left: parent.left
            anchors.top: parent.top
            anchors.bottom: parent.bottom
            width: 9

            color: "transparent"
        }

        ColumnLayout {
            id: contentColumn
            anchors.left: spacer.right
            anchors.right: separatorLine.left
            anchors.top: parent.top
            anchors.topMargin: 7
            anchors.margins: 12

            RowLayout {
                Layout.fillWidth: true

                spacing: 8

                StyledIconLabel {
                    iconCode: Boolean(root.item) ? root.item.icon : 0
                }

                EditableLabel {
                    id: title

                    Layout.preferredHeight: title.implicitHeight
                    Layout.fillWidth: true

                    text: Boolean(root.item) ? root.item.title : ""

                    onTextEdited: function(text) {
                        if (Boolean(root.item)) {
                            root.item.title = text
                        }
                    }
                }

                Loader { id: headerTrailingControls }

                MenuButton {
                    menuModel: contextMenuModel

                    onClicked: {
                        root.selectionRequested(true)
                    }

                    onHandleMenuItem: function(itemId) {
                        contextMenuModel.handleMenuItem(itemId)
                    }
                }
            }

            Loader { 
                id: extraControlsLoader
                Layout.fillWidth: true
                Layout.preferredHeight: implicitHeight
            }
        }

        SeparatorLine {
            id: separatorLine
            anchors.right: rightSideContainer.left
            anchors.bottomMargin: bottomSeparator.thickness
            orientation: Qt.Vertical
        }

        Loader {
            id: rightSideContainer

            anchors.right: parent.right
            anchors.top: parent.top
            anchors.topMargin: 5
            anchors.bottom: parent.bottom
            anchors.bottomMargin: 5

            width: 24
        }
    }

    Item {
        anchors.fill: parent
        MouseArea {
            anchors.fill: parent
            onPressed: function(e) {
                // Pass the event forward to allow
                // child elements to handle the input
                e.accepted = false
                if(!root.isSelected) {
                    root.selectionRequested(false)
                }
            }
        }

        HoverHandler {
            id:hoverHandler
        }
    }

    MouseArea {
        id: resizeArea

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        height: 4

        cursorShape: Qt.SizeVerCursor

        onPressed: {
            root.interactionStarted()
        }

        onPositionChanged: function(mouse) {
            const resizeVerticalMargin = 10
            mouse.accepted = true

            const currentY = mapToItem(container, 0, 0).y - container.y

            const maxPosition = container.height - resizeVerticalMargin - height
            const minPosition = resizeVerticalMargin
            const newPosition = Math.max(Math.min(currentY + mouse.y, maxPosition), minPosition)

            const delta = newPosition - currentY
            trackViewState.changeTrackHeight(delta)
        }

        onReleased: {
            root.interactionEnded()
        }
    }

    Rectangle {
        id: trackHeaderBorder

        anchors.fill: parent
        anchors.rightMargin: -radius
        anchors.leftMargin: spacer.width
        anchors.bottomMargin: bottomSeparator.thickness

        color: "transparent"
        border.width: 1
        border.color: ui.theme.strokeColor

        radius: 4
    }

    SeparatorLine {
        id: bottomSeparator

        anchors.bottom: parent.bottom

        color: "transparent"

        thickness: 2
    }

    Rectangle {
        id: trackFocusState

        anchors.fill: parent
        anchors.leftMargin: spacer.width - border.width
        anchors.rightMargin: -radius
        anchors.topMargin: -border.width
        anchors.bottomMargin: bottomSeparator.thickness - border.width

        visible: root.isFocused

        color: "transparent"

        border.color: "#7EB1FF"
        border.width: 2

        radius: 6
    }
}
