/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.UiComponents 1.0
import Muse.Ui 1.0

import Audacity.Record 1.0
import Audacity.Playback 1.0

StyledPopupView {
    id: root

    property alias volumeLevel: volumeSlider.volumeLevel

    property alias leftCurrentVolumePressure: leftVolumePressure.currentVolumePressure
    property alias leftCurrentRMS: leftVolumePressure.currentRMS

    property alias rightCurrentVolumePressure: rightVolumePressure.currentVolumePressure
    property alias rightCurrentRMS: rightVolumePressure.currentRMS

    property int recordingChannelsCount: 0
    property bool isInputMonitoringOn: false
    property bool isMicMeteringOn: false

    signal volumeLevelChangeRequested(var level)
    signal isInputMonitoringOnChangeRequested(bool enable)
    signal isMicMeteringOnChangeRequested(bool enable)

    QtObject {
        id: prv

        readonly property int contentWidth: 480 - 2 * margins
        readonly property int contentHeight: 210 - 2 * margins

        readonly property int margins: 12
        readonly property int spacing: 8

        readonly property int meterHeight: 50
        readonly property int textHeight: 16
        readonly property int checkboxHeight: 28
    }

    margins: prv.margins

    contentWidth: prv.contentWidth
    contentHeight: prv.contentHeight

    onOpened: {
        leftVolumePressure.requestPaint()
        rightVolumePressure.requestPaint()
    }

    RecordMeterModel {
        id: meterModel
    }

    Component.onCompleted: {
        meterModel.init();
    }

    Column {
        id: content

        anchors.fill: parent

        spacing: prv.spacing

        NavigationPanel {
            id: navPanel
            name: "RecordLevelPopup"
            enabled: root.isOpened
            direction: NavigationPanel.Horizontal
            section: root.navigationSection
        }

        StyledTextLabel {
            anchors.left: parent.left
            anchors.right: parent.right

            height: prv.textHeight

            text: qsTrc("record", "Microphone level")
            horizontalAlignment: Text.AlignLeft
        }

        Rectangle {
            anchors.left: parent.left
            anchors.right: parent.right

            height: prv.meterHeight

            color: ui.theme.backgroundSecondaryColor
            border.width: 1
            border.color: ui.theme.strokeColor
            radius: 2

            Item {
                anchors.fill: parent
                anchors.margins: 12

                Column {
                    id: volumePressureContainer

                    anchors.fill: parent

                    spacing: 2

                    HorizontalVolumePressureMeter {
                        id: leftVolumePressure

                        height: recordingChannelsCount > 1 ? 6 : 14

                        meterModel: meterModel

                        showOverload: false
                    }

                    HorizontalVolumePressureMeter {
                        id: rightVolumePressure

                        meterModel: meterModel

                        showOverload: false

                        visible: recordingChannelsCount > 1
                    }

                    HorizontalVolumePressureRuler {
                        id: recordMeterRuler

                        anchors.left: parent.left
                        anchors.right: parent.right
                        anchors.rightMargin: leftVolumePressure.overloadWidth

                        meterModel: meterModel
                    }
                }

                VolumeSlider {
                    id: volumeSlider

                    meterModel: meterModel

                    anchors.left: parent.left
                    anchors.leftMargin: -handleWidth/2
                    anchors.right: parent.right
                    anchors.rightMargin: -handleWidth/2 + leftVolumePressure.overloadWidth
                    anchors.top: parent.top
                    anchors.topMargin: -1

                    navigation.panel: navPanel
                    navigation.order: 1

                    onVolumeLevelMoved: function(level) {
                        root.volumeLevelChangeRequested(Math.round(level * 100) / 100)
                    }

                    onDecreaseRequested: {
                        if (volumeLevel <= from) {
                            return
                        }
                        root.volumeLevelChangeRequested(Math.round(volumeLevel * 100) / 100 - 1)
                    }

                    onIncreaseRequested: {
                        if (volumeLevel >= to) {
                            return
                        }
                        root.volumeLevelChangeRequested(Math.round(volumeLevel * 100) / 100 + 1)
                    }
                }
            }
        }

        StyledTextLabel {
            anchors.left: parent.left
            anchors.right: parent.right

            height: prv.textHeight

            text: qsTrc("record", "Note: this control is tied to your computer's main mic volume")
            horizontalAlignment: Text.AlignLeft
            wrapMode: Text.WordWrap
        }

        Rectangle {
            anchors.left: parent.left
            anchors.right: parent.right

            height: 8

            color: ui.theme.backgroundPrimaryColor

            SeparatorLine {
                anchors.verticalCenter: parent.verticalCenter
            }
        }

        Column {
            anchors.left: parent.left
            anchors.right: parent.right
            spacing: prv.spacing

            CheckBox {
                id: enableMonitoringCheckbox

                height: prv.checkboxHeight

                text: qsTrc("record", "Turn on input monitoring (hear yourself while recording)")

                checked: root.isInputMonitoringOn

                navigation.panel: navPanel
                navigation.order: 2

                onClicked: {
                    isInputMonitoringOnChangeRequested(!checked)
                }
            }

            CheckBox {
                id: showMeterMeteringCheckbox

                height: prv.checkboxHeight

                text: qsTrc("record", "Show mic metering when not recording")

                checked: root.isMicMeteringOn

                navigation.panel: navPanel
                navigation.order: 3

                onClicked: {
                    isMicMeteringOnChangeRequested(!checked)
                }
            }
        }
    }
}
