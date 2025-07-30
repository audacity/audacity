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

    property bool isPlaying: false

    signal volumeLevelChangeRequested(var level)
    signal widthChangeRequested(int x, int y)

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
        spacing: 0

        FlatButton {
            Layout.preferredWidth: root.height
            Layout.preferredHeight: root.height
            Layout.rightMargin: 6

            icon: IconCode.AUDIO
            accentButton: popup.isOpened

            onClicked: {
                popup.toggleOpened()
            }

            PlaybackMeterCustomisePopup {
                id: popup

                model: playbackMeterModel
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

                    meterModel: playbackMeterModel
                    enabled: root.enabled
                }

                HorizontalVolumePressureMeter {
                    id: rightVolumePressure

                    x: playbackMeterRuler.x + playbackMeterRuler.leftTextMargin
                    width: playbackMeterRuler.effectiveWidth +  leftVolumePressure.overloadTotalSpace

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

                meterModel: playbackMeterModel

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

                    root.volumeLevelChangeRequested(Math.round(level * 100) / 100)
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

        FlatButton {
            id: resizeGrip

            Layout.preferredWidth: 16
            Layout.preferredHeight: root.height
            Layout.leftMargin: 2

            property bool isDragging: false

            mouseArea.cursorShape: Qt.OpenHandCursor
            mouseArea.onPressed: function(e) {
                mouseArea.cursorShape = Qt.ClosedHandCursor;
                resizeGrip.isDragging = true;
            }

            mouseArea.onPositionChanged: function(e) {
                if (resizeGrip.isDragging) {
                    let newPosition = mapToItem(root, e.x, e.y)
                    root.widthChangeRequested(newPosition.x, newPosition.y)
                }
            }

            mouseArea.onReleased: function(e) {
                mouseArea.cursorShape = Qt.OpenHandCursor;
                resizeGrip.isDragging = false;
            }

            transparent: true
            icon: IconCode.DOUBLE_BAR_LINE
        }
    }
}
