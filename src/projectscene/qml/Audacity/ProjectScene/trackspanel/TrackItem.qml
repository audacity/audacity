/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

import "audio"

ListItemBlank {
    id: root

    property var item: null
    property var container: null
    property bool dragged: false
    property bool collapsed: height <= mapFromItem(trackControlsRow, 0, trackControlsRow.height + bottomSeparator.height).y

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

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "Track" + root.item.title + "Panel"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        onActiveChanged: function(active) {
            if (active) {
                root.forceActiveFocus()
            }
        }
    }

    height: trackViewState.trackHeight
    clip: true
    opacity: dragged ? 0.5 : 1

    background.color: (root.isSelected || hoverHandler.hovered) ?
                   ui.theme.backgroundPrimaryColor : ui.theme.backgroundSecondaryColor

    background.opacity: (!root.isSelected || hoverHandler.hovered) ? 0.7 : 1

    signal renameTrackRequested()
    signal duplicateRequested()
    signal deleteRequested()

    signal openEffectsRequested()

    TracksViewStateModel {
        id: trackViewState
        trackId: root.item ? root.item.trackId : -1
    }

    TrackContextMenuModel {
        id: contextMenuModel
        trackId: root.item ? root.item.trackId : -1

        onTrackRenameRequested: title.edit()
    }

    Component.onCompleted: {
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

    RowLayout {
        anchors.fill: parent

        spacing: 0

        TrackSelectionBar {
            id: selectionBar

            isSelected: root.isSelected
            isHovered: hoverHandler.hovered
        }

        ColumnLayout {
            Layout.topMargin: 7
            Layout.margins: 12
            Layout.alignment: Qt.AlignTop
            spacing: 2

            RowLayout {
                Layout.fillWidth: true

                spacing: 8

                StyledIconLabel {
                    iconCode: IconCode.MICROPHONE
                }

                EditableLabel {
                    id: title

                    Layout.preferredHeight: title.implicitHeight
                    Layout.fillWidth: true

                    text: root.item.title

                    onTextEdited: function(text) {
                        root.item.title = text
                    }
                }

                Loader {
                    sourceComponent: trackControlButtons
                    opacity: root.collapsed ? 1 : 0
                    visible: opacity !== 0

                    Behavior on opacity {
                        OpacityAnimator {
                            duration: 100
                        }
                    }
                }

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

            RowLayout {
                id: trackControlsRow

                Layout.fillWidth: true

                spacing: 16

                opacity: root.collapsed ? 0 : 1
                visible: opacity !== 0

                Behavior on opacity {
                    OpacityAnimator {
                        duration: 100
                    }
                }

                BalanceKnob {
                    value: root.item.balance

                    onNewValueRequested: function(newValue) {
                        root.item.balance = newValue
                    }
                }

                VolumeSlider {
                    value: root.item.volumeLevel

                    onValueChanged: {
                        root.item.volumeLevel = value
                    }
                }

                Loader {
                    sourceComponent: trackControlButtons
                }
            }

            FlatButton {
                Layout.fillWidth: true
                Layout.preferredHeight: 24
                Layout.margins: 4

                opacity: root.height > root.mapFromItem(this, 0, height + bottomSeparator.height).y ? 1 : 0
                visible: opacity !== 0

                Behavior on opacity {
                    OpacityAnimator {
                        duration: 100
                    }
                }

                text: qsTrc("projectscene", "Effects")

                onClicked: {
                    root.openEffectsRequested()
                }
            }
        }

        SeparatorLine {}

        Row {
            id: volumePressureContainer
            Layout.alignment: Qt.AlignTop | Qt.AlignHCenter
            Layout.preferredWidth: 24
            Layout.preferredHeight: root.height
            topPadding: 5

            spacing: 2
            Repeater {
                id: volumePressureMeters
                model: root.item.channelCount
                VolumePressureMeter {
                    height: root.height
                    currentVolumePressure: index === 0 ? root.item.leftChannelPressure :
                                                         root.item.rightChannelPressure
                }
            }

            states: [
                State {
                    when: root.item.channelCount === 1
                    name: "mono"
                    PropertyChanges {
                        target: volumePressureContainer
                        leftPadding: 8
                    }
                    PropertyChanges {
                        target: volumePressureMeters.itemAt(0)
                        indicatorWidth: 8
                    }
                },
                State {
                    when: root.item.channelCount === 2
                    name: "stereo"
                    PropertyChanges {
                        target: volumePressureContainer
                        leftPadding: 4
                    }
                    PropertyChanges {
                        target: volumePressureMeters.itemAt(0)
                        indicatorWidth: 7
                    }
                    PropertyChanges {
                        target: volumePressureMeters.itemAt(1)
                        indicatorWidth: 7
                    }
                }
            ]
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

    SeparatorLine {
        id: bottomSeparator

        anchors.bottom: parent.bottom

        thickness: 2
    }

    Component {
        id: trackControlButtons

        RowLayout {

            FlatToggleButton {
                Layout.preferredWidth: 20
                Layout.preferredHeight: Layout.preferredWidth

                icon: IconCode.MUTE
                checked: root.item.muted

                onToggled: {
                    root.item.muted = !checked
                }
            }

            FlatToggleButton {
                Layout.preferredWidth: 20
                Layout.preferredHeight: Layout.preferredWidth

                icon: IconCode.SOLO
                checked: root.item.solo

                onToggled: {
                    root.item.solo = !checked
                }
            }
        }
    }
}
