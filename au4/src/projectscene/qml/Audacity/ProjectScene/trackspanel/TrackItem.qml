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

    height: trackGeometry.trackHeight
    clip: true

    onIsSelectedChanged: {
        if (isSelected && !navigation.active) {
            navigation.requestActive()
        }
    }

    signal duplicateRequested()
    signal deleteRequested()

    signal openEffectsRequested()

    TrackGeometryModel {
        id: trackGeometry
        trackId: root.item ? root.item.trackId : -1
    }

    RowLayout {
        anchors.fill: parent

        spacing: 0

        ColumnLayout {
            Layout.margins: 12

            spacing: 12

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
                                    { "id": "duplicate", "title": qsTrc("notation", "Duplicate") },
                                    { "id": "delete", "title": qsTrc("notation", "Delete") }
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

                StyledSlider {
                    Layout.fillWidth: true

                    value: root.volume
                }

                KnobControl {
                    from: -100
                    to: 100
                    value: root.item.balance
                    stepSize: 1
                    isBalanceKnob: true

                    onNewValueRequested: function(newValue) {
                        root.item.balance = newValue
                    }
                }
            }

            FlatButton {
                Layout.fillWidth: true

                // iconCode

                text: qsTrc("projectscene", "Effects")

                onClicked: {
                    root.openEffectsRequested()
                }
            }
        }

        SeparatorLine {}

        Row {
            Layout.fillHeight: true
            Layout.preferredWidth: childrenRect.width
            Layout.margins: 8

            spacing: 2

            VolumePressureMeter {
                id: leftPressure
                height: parent.height
                currentVolumePressure: root.item.leftChannelPressure
            }

            VolumePressureMeter {
                id: rightPressure
                height: parent.height
                currentVolumePressure: root.item.rightChannelPressure
            }
        }
    }

    MouseArea {
        id: dragArea

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        height: 4

        cursorShape: Qt.SizeVerCursor

        onPositionChanged: function(mouse) {
            trackGeometry.changeTrackHeight(mouse.y)
        }
    }

    SeparatorLine {
        anchors.bottom: parent.bottom
    }
}
