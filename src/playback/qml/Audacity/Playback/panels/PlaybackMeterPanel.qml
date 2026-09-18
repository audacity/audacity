/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene 1.0
import Audacity.Playback 1.0

Item {
    id: root

    property alias navigationSection: navPanel.section
    property alias navigationOrderStart: navPanel.order

    NavigationPanel {
        id: navPanel

        name: "PlaybackMeterPanel"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal

        accessible.name: qsTrc("playback", "Playback meter")
    }

    PlaybackMeterPanelModel {
        id: model

        onIsPlayingChanged: {
            if (model.isPlaying) {
                leftVolumePressure.reset()
                leftVolumePressure.resetClipped()
                rightVolumePressure.reset()
                rightVolumePressure.resetClipped()
            }
        }
    }

    Component.onCompleted: {
        model.init()
    }

    ColumnLayout {
        anchors.fill: parent
        anchors.bottomMargin: 2
        spacing: 0

        FlatButton {
            id: meterOptionsBtn

            Layout.preferredWidth: 28
            Layout.preferredHeight: 28
            Layout.alignment: Qt.AlignHCenter
            Layout.topMargin: 12
            Layout.bottomMargin: 12

            icon: IconCode.AUDIO
            //: Tooltip of the playback meter settings button
            toolTipTitle: qsTrc("playback", "Playback meter settings")
            accentButton: popup.isOpened

            navigation.name: "PlaybackMeterSettings"
            navigation.panel: navPanel
            navigation.order: 1

            onClicked: {
                popup.toggleOpened()
            }

            PlaybackMeterCustomisePopup {
                id: popup

                placementPolicies: PopupView.PreferLeft
                y: -28

                model: model.meterModel
            }
        }

        Item {
            id: meterContainer

            Layout.fillHeight: true
            Layout.preferredWidth: root.width

            Row {
                id: meterChannelRow

                spacing: 2

                anchors.fill: parent
                anchors.leftMargin: 4
                anchors.rightMargin: 4
                anchors.topMargin: 2
                anchors.bottomMargin: 6

                VolumePressureMeter {
                    id: leftVolumePressure

                    anchors.top: parent.top
                    height: parent.height - ruler.bottomTextMargin

                    overloadHeight: 10

                    currentVolumePressure: model.leftChannelPressure
                    currentRMS: model.leftChannelRMS

                    meterModel: model.meterModel

                    indicatorWidth: 10
                }

                VolumePressureMeter {
                    id: rightVolumePressure

                    anchors.top: parent.top
                    height: parent.height - ruler.bottomTextMargin

                    overloadHeight: 10

                    currentVolumePressure: model.rightChannelPressure
                    currentRMS: model.rightChannelRMS

                    meterModel: model.meterModel

                    indicatorWidth: 10
                }

                VolumePressureRuler {
                    id: ruler

                    meterModel: model.meterModel

                    anchors.top: parent.top
                    anchors.topMargin: leftVolumePressure.overloadHeight - topTextMargin
                    anchors.bottom: parent.bottom
                }
            }

            VerticalVolumeSlider {
                id: volumeSlider

                orientation: Qt.Vertical

                meterModel: model.meterModel

                volumeLevel: model.level

                anchors.top: parent.top
                anchors.topMargin: 2 + leftVolumePressure.overloadHeight - (handleWidth / 2)
                anchors.bottom: parent.bottom
                anchors.left: parent.left
                anchors.leftMargin: 3

                handleWidth: 24

                navigation.name: "PlaybackVolume"
                navigation.panel: navPanel
                navigation.order: meterOptionsBtn.navigation.order + 1

                onVolumeLevelMoved: function (level) {
                    leftVolumePressure.reset()
                    leftVolumePressure.resetClipped()
                    rightVolumePressure.reset()
                    rightVolumePressure.resetClipped()

                    model.volumeLevelChangeRequested(Math.round(level * 100) / 100)
                }

                onHandlePressed: function () {
                    leftVolumePressure.reset()
                    leftVolumePressure.resetClipped()
                    rightVolumePressure.reset()
                    rightVolumePressure.resetClipped()
                }

                onDecreaseRequested: {
                    if (volumeLevel <= from) {
                        return
                    }
                    model.volumeLevelChangeRequested(Math.round(volumeLevel * 100) / 100 - stepSize)
                }

                onIncreaseRequested: {
                    if (volumeLevel >= to) {
                        return
                    }
                    model.volumeLevelChangeRequested(Math.round(volumeLevel * 100) / 100 + stepSize)
                }
            }

            MouseArea {
                id: overloadClickArea

                anchors.top: meterChannelRow.top
                anchors.left: meterContainer.left
                anchors.right: meterContainer.right

                height: leftVolumePressure.overloadHeight + 2

                z: 10

                // This will avoid to reset the volume levels when clicking on the overload area
                enabled: (volumeSlider.handleY >= leftVolumePressure.overloadHeight)

                onClicked: function (mouse) {
                    leftVolumePressure.reset()
                    leftVolumePressure.resetClipped()
                    rightVolumePressure.reset()
                    rightVolumePressure.resetClipped()
                }
            }
        }
    }
}
