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

TrackItem {
    id: root

    headerTrailingControlsComponent: Component {
        RowLayout {
            opacity: root.collapsed ? 1 : 0
            visible: opacity !== 0

            Behavior on opacity {
                OpacityAnimator {
                    duration: 100
                }
            }

            Loader {
                sourceComponent: trackControlButtons
            }
        }
    }

    extraControlsComponent: Component {
        ColumnLayout {
            spacing: 2

            RowLayout {
                id: topRow

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
                    id: panKnob

                    value: Boolean(root.item) ? root.item.pan : 0

                    navigation.panel: root.headerNavigationPanel
                    navigation.order: root.extraControlsNavigationStart
                    navigation.enabled: !root.collapsed
                    //: Noun: accessibility name of the pan knob
                    navigation.accessible.name: qsTrc("projectscene", "Pan")

                    onNewPanRequested: function (newValue, completed) {
                        if (Boolean(root.item)) {
                            root.item.setPan(newValue, completed)
                        }
                    }
                }

                VolumeSlider {
                    id: volumeSlider

                    value: Boolean(root.item) ? root.item.volumeLevel : 0

                    navigation.panel: root.headerNavigationPanel
                    navigation.order: panKnob.navigation.order + 1
                    navigation.enabled: !root.collapsed
                    navigation.accessible.name: qsTrc("projectscene", "Track volume")

                    onNewVolumeRequested: function (newValue, completed) {
                        if (Boolean(root.item)) {
                            root.item.setVolumeLevel(newValue, completed)
                        }
                    }

                    Component.onCompleted: {
                        root.extraControlsNavigationEnd = Qt.binding(function () {
                            return volumeSlider.navigation.order
                        })
                    }
                }

                Loader {
                    sourceComponent: trackControlButtons
                }
            }

            RowLayout {
                id: bottomRow

                Layout.fillWidth: true
                Layout.preferredHeight: 24

                spacing: 4

                opacity: topRow.visible && root.height > root.mapFromItem(this, 0, height + bottomSeparatorHeight).y ? 1 : 0
                visible: opacity !== 0
                Behavior on opacity {
                    OpacityAnimator {
                        duration: 100
                    }
                }

                FlatButton {
                    Layout.fillWidth: true
                    Layout.preferredHeight: 24

                    text: qsTrc("projectscene", "Effects")
                    toolTipDescription: qsTrc("projectscene", "Opens effects panel")

                    navigation.panel: root.headerNavigationPanel
                    navigation.order: root.headerTrailingControlsNavigationEnd + 1
                    navigation.enabled: bottomRow.visible

                    onClicked: {
                        root.openEffectsRequested()
                    }
                }

                FlatButton {
                    id: outputButton

                    Layout.fillWidth: true
                    Layout.preferredHeight: 24

                    // Only once there are busses to go to
                    visible: Boolean(root.item) && root.item.hasOutputChoice

                    text: Boolean(root.item) ? root.item.outputTitle : ""
                    //: Where a track's audio goes: the master, or a bus
                    toolTipTitle: qsTrc("projectscene", "Output")
                    toolTipDescription: qsTrc("projectscene", "Sends this track to the master or to a bus")

                    navigation.panel: root.headerNavigationPanel
                    navigation.order: root.headerTrailingControlsNavigationEnd + 2
                    navigation.enabled: bottomRow.visible && visible
                    navigation.accessible.name: qsTrc("projectscene", "Output: %1").arg(text)

                    onClicked: {
                        outputMenuLoader.toggleOpened(root.item.outputMenu())
                    }

                    StyledMenuLoader {
                        id: outputMenuLoader

                        onHandleMenuItem: function (itemId) {
                            root.item.setOutput(itemId)
                        }
                    }
                }

                FlatButton {
                    id: sendsButton

                    Layout.fillWidth: true
                    Layout.preferredHeight: 24

                    // Only once there are busses to send to
                    visible: outputButton.visible

                    //: What a track sends to busses besides its output
                    text: qsTrc("projectscene", "Sends")
                    toolTipDescription: qsTrc("projectscene", "Sends this track to busses besides its output")

                    navigation.panel: root.headerNavigationPanel
                    navigation.order: root.headerTrailingControlsNavigationEnd + 3
                    navigation.enabled: bottomRow.visible && visible

                    onClicked: {
                        if (sendsPopup.isOpened) {
                            sendsPopup.close()
                        } else {
                            sendsPopup.open()
                        }
                    }

                    SendsPopup {
                        id: sendsPopup

                        trackId: Boolean(root.item) ? root.item.trackId : null
                    }
                }
            }
        }
    }

    rightSideContainerComponent: Component {
        Item {
            width: parent.width
            height: parent.height

            Row {
                id: volumePressureContainer

                property int indicatorWidth: 7

                anchors.top: parent.top
                anchors.bottom: parent.bottom
                anchors.horizontalCenter: parent.horizontalCenter

                spacing: 2

                function clearMeters() {
                    leftOrMonoVolumePressureMeter.reset()
                    leftOrMonoVolumePressureMeter.resetClipped()
                    rightVolumePressureMeter.reset()
                    rightVolumePressureMeter.resetClipped()
                }

                TapHandler {
                    onTapped: volumePressureContainer.clearMeters()
                }

                VolumePressureMeter {
                    id: leftOrMonoVolumePressureMeter
                    anchors.top: parent.top
                    anchors.bottom: parent.bottom
                    indicatorWidth: parent.indicatorWidth
                    meterModel: trackViewState.meterModel
                    currentVolumePressure: Boolean(root.item) ? root.item.leftChannelPressure : 0
                    currentRMS: Boolean(root.item) ? root.item.leftChannelRMS : 0
                }

                VolumePressureMeter {
                    id: rightVolumePressureMeter
                    anchors.top: parent.top
                    anchors.bottom: parent.bottom
                    indicatorWidth: parent.indicatorWidth
                    meterModel: trackViewState.meterModel
                    currentVolumePressure: Boolean(root.item) ? root.item.rightChannelPressure : 0
                    currentRMS: Boolean(root.item) ? root.item.rightChannelRMS : 0
                }

                Connections {
                    target: root.trackViewState

                    function onIsRecordingChanged() {
                        if (trackViewState.isRecording) {
                            leftOrMonoVolumePressureMeter.resetClipped()
                            rightVolumePressureMeter.resetClipped()
                        }

                        leftOrMonoVolumePressureMeter.reset()
                        rightVolumePressureMeter.reset()
                    }

                    function onIsPlayingChanged() {
                        if (trackViewState.isPlaying) {
                            volumePressureContainer.clearMeters()
                        }
                    }
                }

                Connections {
                    target: root

                    function onIsFocusedChanged() {
                        if (!root.isFocused) {
                            volumePressureContainer.clearMeters()
                        }
                    }
                }
            }

            states: [
                State {
                    when: Boolean(root.item) && root.item.channelCount === 1
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
                    when: Boolean(root.item) && root.item.channelCount === 2
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

    Component {
        id: trackControlButtons

        RowLayout {
            spacing: 4

            FlatToggleButton {
                id: muteButton

                Layout.preferredWidth: 20
                Layout.preferredHeight: Layout.preferredWidth

                icon: IconCode.MUTE
                checked: Boolean(root.item) ? root.item.muted : false

                navigation.panel: root.headerNavigationPanel
                navigation.order: root.headerTrailingControlsNavigationStart
                //: Accessibility name of the mute button
                navigation.accessible.name: qsTrc("projectscene", "Mute")
                navigation.accessible.role: MUAccessible.CheckBox

                onToggled: {
                    if (Boolean(root.item)) {
                        root.item.muted = !checked
                    }
                }
            }

            FlatToggleButton {
                id: soloButton

                Layout.preferredWidth: 20
                Layout.preferredHeight: Layout.preferredWidth

                icon: IconCode.SOLO
                checked: Boolean(root.item) ? root.item.solo : false

                navigation.panel: root.headerNavigationPanel
                navigation.order: muteButton.navigation.order + 1
                //: Accessibility name of the solo button
                navigation.accessible.name: qsTrc("projectscene", "Solo")
                navigation.accessible.role: MUAccessible.CheckBox

                onToggled: {
                    if (Boolean(root.item)) {
                        root.item.solo = !checked
                    }
                }

                Component.onCompleted: {
                    root.headerTrailingControlsNavigationEnd = Qt.binding(function () {
                        return soloButton.navigation.order
                    })
                }
            }
        }
    }
}
