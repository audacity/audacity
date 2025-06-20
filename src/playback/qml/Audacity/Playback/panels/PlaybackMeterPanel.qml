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
        anchors.bottomMargin: 2

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
            id: meterContainer

            Layout.fillHeight: true
            Layout.preferredWidth: root.width

            Row {
                id: meterChannelRow

                spacing: 2

                anchors.fill: parent
                anchors.leftMargin: 4
                anchors.rightMargin: 4
                anchors.topMargin: 14
                anchors.bottomMargin: 11

                VolumePressureMeter {
                    id: leftVolumePressure

                    anchors.top: parent.top
                    anchors.bottom: parent.bottom
                    anchors.bottomMargin: ruler.bottomTextMargin

                    overloadHeight: 10

                    currentVolumePressure: model.leftChannelPressure
                    currentRMS: model.leftChannelRMS

                    meterStyle: model.meterStyle
                    meterModel: model.meterModel

                    indicatorWidth: 10
                }

                VolumePressureMeter {
                    id: rightVolumePressure


                    anchors.top: parent.top
                    anchors.bottom: parent.bottom
                    anchors.bottomMargin: ruler.bottomTextMargin

                    overloadHeight: 10

                    currentVolumePressure: model.rightChannelPressure
                    currentRMS: model.rightChannelRMS

                    meterStyle: model.meterStyle
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

                volumeLevel: model.level

                anchors.top: parent.top
                anchors.topMargin: 14  + leftVolumePressure.overloadHeight - (handleWidth / 2)
                anchors.bottom: parent.bottom
                anchors.bottomMargin: ruler.bottomTextMargin
                anchors.left: parent.left
                anchors.leftMargin: 3

                handleWidth: 24

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

            MouseArea {
                id: overloadClickArea

                anchors.top: meterChannelRow.top
                anchors.left: meterContainer.left
                anchors.right: meterContainer.right

                height: leftVolumePressure.overloadHeight + 2

                z: 10

                // This will avoid to reset the volume levels when clicking on the overload area
                enabled: (volumeSlider.handleY >= leftVolumePressure.overloadHeight)

                onClicked: function(mouse) {
                    leftVolumePressure.reset()
                    leftVolumePressure.resetClipped()
                    rightVolumePressure.reset()
                    rightVolumePressure.resetClipped()
                }
            }
        }
    }
}