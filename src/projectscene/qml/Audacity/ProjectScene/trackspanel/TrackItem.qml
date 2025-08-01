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
    property bool collapsed: trackViewState.isTrackCollapsed

    property bool isFocused: false

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
    opacity: dragged ? 0.5 : 1

    background.color: (root.isSelected || hoverHandler.hovered) ?
                   ui.theme.backgroundPrimaryColor : ui.theme.backgroundSecondaryColor

    background.opacity: (!root.isSelected || hoverHandler.hovered) ? 0.7 : 1

    signal renameTrackRequested()
    signal duplicateRequested()
    signal deleteRequested()

    signal openEffectsRequested()

    TrackViewStateModel {
        id: trackViewState
        trackId: root.item ? root.item.trackId : -1

        onIsPlayingChanged: {
            if (trackViewState.isPlaying) {
                leftOrMonoVolumePressureMeter.reset()
                leftOrMonoVolumePressureMeter.resetClipped()
                rightVolumePressureMeter.reset()
                rightVolumePressureMeter.resetClipped()
            }
        }

        onIsRecordingChanged: {
            if (trackViewState.isRecording) {
                leftOrMonoVolumePressureMeter.resetClipped()
                rightVolumePressureMeter.resetClipped()
            }

            leftOrMonoVolumePressureMeter.reset()
            rightVolumePressureMeter.reset()
        }
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

        Rectangle {
            id: spacer

            color: "transparent"

            Layout.fillHeight: true
            Layout.preferredWidth: 9
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

                PanKnob {
                    value: root.item.pan

                    onNewPanRequested: function(newValue, completed) {
                        root.item.setPan(newValue, completed)
                    }
                }

                VolumeSlider {
                    value: root.item.volumeLevel

                    onNewVolumeRequested: function(newValue, completed) {
                        root.item.setVolumeLevel(newValue, completed)
                    }
                }

                Loader {
                    sourceComponent: trackControlButtons
                }
            }

            FlatButton {
                Layout.fillWidth: true
                Layout.preferredHeight: 24

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

        SeparatorLine {
            Layout.bottomMargin: 2 * bottomSeparator.thickness
        }

        Item {
            Layout.fillHeight: true
            Layout.topMargin: 5
            Layout.bottomMargin: 5
            width: 24

            Row {
                id: volumePressureContainer

                anchors.top: parent.top
                anchors.bottom: parent.bottom
                anchors.horizontalCenter: parent.horizontalCenter

                property int indicatorWidth: 7

                spacing: 2

                TapHandler {
                    id: volumePressureTapHandler
                    onTapped: {
                        rightVolumePressureMeter.reset()
                        rightVolumePressureMeter.resetClipped()
                        leftOrMonoVolumePressureMeter.reset()
                        leftOrMonoVolumePressureMeter.resetClipped()
                    }
                }

                VolumePressureMeter {
                    id: leftOrMonoVolumePressureMeter

                    anchors.top: parent.top
                    anchors.bottom: parent.bottom

                    indicatorWidth: parent.indicatorWidth

                    meterModel: trackViewState.meterModel

                    currentVolumePressure: root.item.leftChannelPressure
                    currentRMS: root.item.leftChannelRMS
                }

                VolumePressureMeter {
                    id: rightVolumePressureMeter

                    anchors.top: parent.top
                    anchors.bottom: parent.bottom

                    indicatorWidth: parent.indicatorWidth

                    meterModel: trackViewState.meterModel

                    currentVolumePressure: root.item.rightChannelPressure
                    currentRMS: root.item.rightChannelRMS
                }
            }

            states: [
                State {
                    when: root.item.channelCount === 1
                    name: "mono"
                    PropertyChanges {
                        target: volumePressureContainer
                        indicatorWidth: 8
                    }
                    PropertyChanges {
                        target: rightVolumePressureMeter
                        visible: false
                    }
                },
                State {
                    when: root.item.channelCount === 2
                    name: "stereo"
                    PropertyChanges {
                        target: volumePressureContainer
                        indicatorWidth: 7
                    }
                    PropertyChanges {
                        target: rightVolumePressureMeter
                        visible: true
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
