/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.UiComponents 1.0
import Muse.Ui 1.0

import Audacity.Record 1.0
import Audacity.Playback 1.0

FlatButton {
    id: root

    property alias volumeLevel: volumeSlider.volumeLevel

    property alias leftCurrentVolumePressure: leftVolumePressure.currentVolumePressure
    property alias leftCurrentRMS: leftVolumePressure.currentRMS

    property alias rightCurrentVolumePressure: rightVolumePressure.currentVolumePressure
    property alias rightCurrentRMS: rightVolumePressure.currentRMS

    property int meterStyle: PlaybackMeterStyle.Default

    property NavigationPanel navigationPanel: null
    property int navigationOrder: 0

    property int recordingChannelsCount: 0
    property bool isInputMonitoringOn: false
    property bool isMicMeteringOn: false

    signal volumeLevelChangeRequested(var level)
    signal isInputMonitoringOnChangeRequested(bool enable)
    signal isMicMeteringOnChangeRequested(bool enable)
    signal isPopupOpened(bool opened)

    accentButton: popup.isOpened

    iconFont: ui.theme.toolbarIconsFont

    navigation.panel: navigationPanel
    navigation.name: text
    navigation.order: navigationOrder
    isClickOnKeyNavTriggered: false
    navigation.onTriggered: {
        togglePopupOpened()
    }

    mouseArea.acceptedButtons: Qt.LeftButton | Qt.RightButton

    function togglePopupOpened() {
        if (popup.isOpened) {
            popup.close()
        } else {
            popup.open()
        }
    }

    onClicked: function (mouse) {
        togglePopupOpened()
    }

    Connections {
        target: root.mouseArea

        enabled: !popup.isOpened

        function onPressAndHold() {
            if (popup.isOpened) {
                return
            }

            root.togglePopupOpened()
        }
    }

    QtObject {
        id: viewModel

        readonly property int contentWidth: 480 - 2 * margins
        readonly property int contentHeight: 174 - 2 * margins

        readonly property int margins: ui.theme.extra.space_12
        readonly property int checkboxSpacing: ui.theme.extra.space_20

        readonly property int meterHeight: 50
        readonly property int textHeight: 16
        readonly property int checkboxHeight: 28
    }

    RecordMeterModel {
        id: meterModel
    }

    Component.onCompleted: {
        meterModel.init()
    }

    StyledPopupView {
        id: popup

        margins: viewModel.margins

        contentWidth: viewModel.contentWidth
        contentHeight: viewModel.contentHeight

        onOpened: {
            leftVolumePressure.requestPaint()
            rightVolumePressure.requestPaint()

            root.isPopupOpened(true)
        }

        onClosed: {
            root.isPopupOpened(false)
        }

        Column {
            id: content

            anchors.fill: parent

            spacing: ui.theme.extra.space_8

            StyledTextLabel {
                anchors.left: parent.left
                anchors.right: parent.right

                height: viewModel.textHeight

                text: qsTrc("record", "Microphone level")
                horizontalAlignment: Text.AlignLeft
            }

            Rectangle {
                anchors.left: parent.left
                anchors.right: parent.right

                height: viewModel.meterHeight

                color: ui.theme.backgroundSecondaryColor
                border.width: 1
                border.color: ui.theme.strokeColor
                radius: 2

                Item {
                    anchors.fill: parent
                    anchors.margins: ui.theme.extra.space_12

                    Column {
                        id: volumePressureContainer

                        anchors.fill: parent

                        spacing: ui.theme.extra.space_2

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
                        anchors.leftMargin: -handleWidth / 2
                        anchors.right: parent.right
                        anchors.rightMargin: -handleWidth / 2 + leftVolumePressure.overloadWidth
                        anchors.top: parent.top
                        anchors.topMargin: -1

                        onVolumeLevelMoved: function (level) {
                            root.volumeLevelChangeRequested(Math.round(level * 100) / 100)
                        }
                    }
                }
            }

            StyledTextLabel {
                anchors.left: parent.left
                anchors.right: parent.right

                height: viewModel.textHeight

                text: qsTrc("record", "Note: this control is tied to your computer’s main mic volume")
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

            Row {
                anchors.left: parent.left
                anchors.right: parent.right

                height: viewModel.checkboxHeight

                spacing: viewModel.checkboxSpacing

                CheckBox {
                    id: showMeterMeteringCheckbox

                    anchors.verticalCenter: parent.verticalCenter

                    text: qsTrc("record", "Show mic metering")

                    checked: root.isMicMeteringOn

                    onClicked: {
                        isMicMeteringOnChangeRequested(!checked)
                    }
                }

                CheckBox {
                    id: enableMonitoringCheckbox

                    anchors.verticalCenter: parent.verticalCenter

                    text: qsTrc("record", "Enable input monitoring")

                    checked: root.isInputMonitoringOn

                    onClicked: {
                        isInputMonitoringOnChangeRequested(!checked)
                    }
                }
            }
        }
    }
}
