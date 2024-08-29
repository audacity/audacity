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

    signal interactionStarted()
    signal interactionEnded()

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

    signal duplicateRequested()
    signal deleteRequested()

    signal openEffectsRequested()

    TracksViewStateModel {
        id: trackViewState
        trackId: root.item ? root.item.trackId : -1
    }

    Component.onCompleted: {
        trackViewState.init()
    }

    RowLayout {
        anchors.fill: parent

        spacing: 0

        TrackSelectionBar {
            id: selectionBar
            isSelected: root.isSelected
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

                StyledTextLabel {
                    Layout.fillWidth: true

                    text: root.item.title
                    font: ui.theme.bodyBoldFont
                    horizontalAlignment: Text.AlignLeft
                }

                MenuButton {
                    Component.onCompleted: {
                        var operations = [
                                    { "id": "duplicate", "title": qsTrc("track", "Duplicate") },
                                    { "id": "delete", "title": qsTrc("track", "Delete") }
                                ]

                        menuModel = operations
                    }

                    onHandleMenuItem: function(itemId) {
                        switch(itemId) {
                        case "duplicate":
                            root.duplicateRequested()
                            break
                        case "delete":
                            root.deleteRequested()
                            break
                        }
                    }
                }
            }

            RowLayout {
                Layout.fillWidth: true

                spacing: 16

                BalanceKnob {
                    value: root.item.balance

                    onNewValueRequested: function(newValue) {
                        root.item.balance = newValue
                    }
                }

                StyledSlider {
                    Layout.fillWidth: true

                    value: root.item.volumeLevel
                }


                RowLayout {
                    Layout.fillWidth: true

                    spacing: 4

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

            FlatButton {
                Layout.fillWidth: true
                Layout.preferredHeight: 24
                Layout.margins: 4

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

    MouseArea {
        id: dragArea

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        height: 4

        cursorShape: Qt.SizeVerCursor

        onPressed: {
            root.interactionStarted()
        }

        onPositionChanged: function(mouse) {
            mouse.accepted = true
            trackViewState.changeTrackHeight(mouse.y)
        }

        onReleased: {
            root.interactionEnded()
        }
    }

    SeparatorLine {
        anchors.bottom: parent.bottom
        separatorWidth: 2
    }
}
