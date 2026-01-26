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

            Behavior on opacity { OpacityAnimator { duration: 100 } }

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
                Behavior on opacity { OpacityAnimator { duration: 100 } }

                PanKnob {
                    value: Boolean(root.item) ? root.item.pan : 0
                    onNewPanRequested: function(newValue, completed) {
                        if (Boolean(root.item)) {
                            root.item.setPan(newValue, completed)
                        }
                    }
                }

                VolumeSlider {
                    value: Boolean(root.item) ? root.item.volumeLevel : 0
                    onNewVolumeRequested: function(newValue, completed) {
                        if (Boolean(root.item)) {
                            root.item.setVolumeLevel(newValue, completed)
                        }
                    }
                }

                Loader {
                    sourceComponent: trackControlButtons
                }
            }

            FlatButton {
                Layout.fillWidth: true
                Layout.preferredHeight: 24

                opacity: topRow.visible && root.height > root.mapFromItem(this, 0, height + bottomSeparatorHeight).y ? 1 : 0
                visible: opacity !== 0
                Behavior on opacity { OpacityAnimator { duration: 100 } }

                text: qsTrc("projectscene", "Effects")

                onClicked: {
                    root.openEffectsRequested()
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

                TapHandler { onTapped: volumePressureContainer.clearMeters() }

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
                    PropertyChanges { target: volumePressureContainer; indicatorWidth: 8 }
                    PropertyChanges { target: rightVolumePressureMeter; visible: false }
                },
                State {
                    when: Boolean(root.item) && root.item.channelCount === 2
                    name: "stereo"
                    PropertyChanges { target: volumePressureContainer; indicatorWidth: 7 }
                    PropertyChanges { target: rightVolumePressureMeter; visible: true }
                }
            ]
        }
    }

    Component {
        id: trackControlButtons

        RowLayout {
            spacing: 4

            FlatToggleButton {
                Layout.preferredWidth: 20
                Layout.preferredHeight: Layout.preferredWidth

                icon: IconCode.MUTE
                checked: Boolean(root.item) ? root.item.muted : false

                onToggled: {
                    if (Boolean(root.item)) {
                        root.item.muted = !checked
                    }
                }
            }

            FlatToggleButton {
                Layout.preferredWidth: 20
                Layout.preferredHeight: Layout.preferredWidth

                icon: IconCode.SOLO
                checked: Boolean(root.item) ? root.item.solo : false

                onToggled: {
                    if (Boolean(root.item)) {
                        root.item.solo = !checked
                    }
                }
            }
        }
    }
}
