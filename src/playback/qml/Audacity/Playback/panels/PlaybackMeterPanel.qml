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

    PlaybackMeterPanelModel {
        id: model

        onIsPlayingChanged: {
            if (model.isPlaying) {
                leftVolumePressure.reset()
                leftVolumePressure.resetClipped()
                rightVolumePressure.reset()
                rightVolumePressure.resetClipped();
            }
        }
    }

    ColumnLayout {
        anchors.fill: parent

        FlatButton {
            id: meterOptionsBtn

            Layout.preferredWidth: 28
            Layout.preferredHeight: 28
            Layout.alignment: Qt.AlignHCenter
            Layout.topMargin: 6

            icon: IconCode.AUDIO
            accentButton: popup.isOpened

            onClicked: {
                popup.toggleOpened()
            }

            PlaybackMeterCustomisePopup {
                id: popup

                meterStyle: model.meterStyle
                meterType: model.meterType
                meterPosition: model.meterPosition

                onPositionChangeRequested: function (position) {
                    model.positionChangeRequested(position)
                }

                onStyleChangeRequested: function (style) {
                    model.styleChangeRequested(style)
                }

                onTypeChangeRequested: function (type) {
                    model.typeChangeRequested(type)
                }
            }
        }

        Item {
            Layout.fillHeight: true
            Layout.preferredWidth: root.width

            Row {
                id: meterChannelRow

                topPadding: volumeSlider.handleHeight /  2

                spacing: 2

                anchors.horizontalCenter: parent.horizontalCenter
                anchors.top: parent.top
                anchors.bottom: parent.bottom

                VolumePressureMeter {
                    id: leftVolumePressure

                    overloadHeight: 10

                    currentVolumePressure: model.leftChannelPressure
                    currentRMS: model.leftChannelRMS

                    meterStyle: model.meterStyle

                    height: parent.height - volumeSlider.handleHeight / 2
                    indicatorWidth: 11
                }

                VolumePressureMeter {
                    id: rightVolumePressure

                    overloadHeight: 10

                    currentVolumePressure: model.rightChannelPressure
                    currentRMS: model.rightChannelRMS

                    meterStyle: model.meterStyle

                    height: parent.height - volumeSlider.handleHeight / 2
                    indicatorWidth: 11
                    showRuler: true
                }
            }

            VerticalVolumeSlider {
                id: volumeSlider

                orientation: Qt.Vertical

                volumeLevel: model.level

                anchors.top: parent.top
                anchors.topMargin: leftVolumePressure.overloadHeight
                anchors.bottom: parent.bottom
                anchors.left: parent.left
                anchors.leftMargin: 6

                handleWidth: 18

                onVolumeLevelMoved: function(level) {
                    leftVolumePressure.reset()
                    leftVolumePressure.resetClipped()
                    rightVolumePressure.reset()
                    rightVolumePressure.resetClipped()

                    model.volumeLevelChangeRequested(level)
                }

                onHandlePressed: function() {
                    leftVolumePressure.reset()
                    leftVolumePressure.resetClipped()
                    rightVolumePressure.reset()
                    rightVolumePressure.resetClipped()
                }
            }
        }
    }
}