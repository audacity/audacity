/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.UiComponents 1.0
import Muse.Ui 1.0

import Audacity.Playback 1.0

Item {
    id: root

    property alias volumeLevel: volumeSlider.volumeLevel

    property alias leftCurrentVolumePressure: leftVolumePressure.currentVolumePressure
    property alias leftCurrentRMS: leftVolumePressure.currentRMS

    property alias rightCurrentVolumePressure: rightVolumePressure.currentVolumePressure
    property alias rightCurrentRMS: rightVolumePressure.currentRMS

    property NavigationPanel navigationPanel: null
    property int navigationOrder: 0

    property int meterStyle: PlaybackMeterStyle.Default
    property int meterType: PlaybackMeterType.DbLog
    property int meterPosition: PlaybackMeterPosition.TopBar

    property bool isPlaying: false

    signal volumeLevelChangeRequested(var level)
    signal positionChangeRequested(int position)
    signal styleChangeRequested(int style)
    signal typeChangeRequested(int type)

    onIsPlayingChanged: {
        if (root.isPlaying) {
            leftVolumePressure.reset()
            leftVolumePressure.resetClipped()
            rightVolumePressure.reset()
            rightVolumePressure.resetClipped();
        }
    }

    PlaybackMeterModel {
        id: playbackMeterModel
    }

    RowLayout {
        anchors.fill: parent
        spacing: 6

        FlatButton {
            Layout.preferredWidth: root.height
            Layout.preferredHeight: root.height

            icon: IconCode.AUDIO
            accentButton: popup.isOpened

            onClicked: {
                popup.toggleOpened()
            }

            PlaybackMeterCustomisePopup {
                id: popup

                meterStyle: root.meterStyle
                meterType: root.meterType
                meterPosition: root.meterPosition

                onPositionChangeRequested: function (position) {
                    root.positionChangeRequested(position)
                }

                onStyleChangeRequested: function (style) {
                    root.styleChangeRequested(style)
                }

                onTypeChangeRequested: function (type) {
                    root.typeChangeRequested(type)
                }
            }
        }

        Item {
            Layout.fillWidth: true
            Layout.preferredHeight: root.height

            Column {
                id: volumePressureContainer

                anchors.fill: parent
                anchors.topMargin: 2

                spacing: 2

                HorizontalVolumePressureMeter {
                    id: leftVolumePressure

                    x: playbackMeterRuler.x + playbackMeterRuler.leftTextMargin
                    width: playbackMeterRuler.effectiveWidth + leftVolumePressure.overloadTotalSpace

                    meterStyle: root.meterStyle
                    meterModel: playbackMeterModel
                    enabled: root.enabled
                }

                HorizontalVolumePressureMeter {
                    id: rightVolumePressure

                    x: playbackMeterRuler.x + playbackMeterRuler.leftTextMargin
                    width: playbackMeterRuler.effectiveWidth +  leftVolumePressure.overloadTotalSpace

                    meterStyle: root.meterStyle
                    meterModel: playbackMeterModel
                    enabled: root.enabled
                }

                HorizontalVolumePressureRuler {
                    id: playbackMeterRuler

                    meterModel: playbackMeterModel

                    anchors.left: parent.left
                    anchors.right: parent.right
                    anchors.rightMargin: leftVolumePressure.overloadWidth
                }
            }

            VolumeSlider {
                id: volumeSlider

                anchors.left: parent.left
                anchors.leftMargin: playbackMeterRuler.leftTextMargin - volumeSlider.handleWidth / 2
                anchors.right: parent.right
                anchors.rightMargin: leftVolumePressure.overloadTotalSpace
                anchors.top: parent.top
                anchors.topMargin: 1

                enabled: root.enabled

                navigation.panel: root.navigationPanel
                navigation.order: root.navigationOrder

                onVolumeLevelMoved: function(level) {
                    leftVolumePressure.reset()
                    leftVolumePressure.resetClipped()
                    rightVolumePressure.reset()
                    rightVolumePressure.resetClipped()

                    root.volumeLevelChangeRequested(Math.round(level * 10) / 10)
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

                anchors.top: volumePressureContainer.top
                anchors.bottom: volumePressureContainer.bottom
                anchors.right: volumePressureContainer.right

                width: volumeSlider.handleWidth

                z: 10

                enabled:  volumeSlider.handleX < (volumeSlider.width - volumeSlider.handleWidth - leftVolumePressure.overloadWidth)

                onClicked: {
                    leftVolumePressure.reset()
                    leftVolumePressure.resetClipped()
                    rightVolumePressure.reset()
                    rightVolumePressure.resetClipped()
                }
            }
        }
    }
}
