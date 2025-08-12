/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.UiComponents 1.0
import Muse.Ui 1.0

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

    signal volumeLevelChangeRequested(var level)

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

    onClicked: function(mouse) {
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

        readonly property int contentWidth: 456
        readonly property int contentHeight: 142

        readonly property int margins: 12
        readonly property int spacing: 8

        readonly property int meterHeight: 50
    }

    PlaybackMeterModel {
        id: meterModel

        meterInputSource: MeterInputSource.Record
    }

    StyledPopupView {
        id: popup

        margins: viewModel.margins

        contentWidth: viewModel.contentWidth
        contentHeight: viewModel.contentHeight

        onOpened: {
            leftVolumePressure.requestPaint()
            rightVolumePressure.requestPaint()
        }

        ColumnLayout {
            id: content

            anchors.fill: parent

            spacing: viewModel.spacing

            StyledTextLabel {
                Layout.fillWidth: true

                text: qsTrc("record", "Microphone level")
                horizontalAlignment: Text.AlignLeft
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: viewModel.meterHeight

                color: "#EBEDF0"
                border.width: 1
                border.color: ui.theme.strokeColor
                radius: 2

                Item {
                    anchors.fill: parent
                    anchors.margins: 8

                    Column {
                        id: volumePressureContainer

                        anchors.fill: parent

                        spacing: 2

                        HorizontalVolumePressureMeter {
                            id: leftVolumePressure

                            meterModel: meterModel

                            showOverload: false
                        }

                        HorizontalVolumePressureMeter {
                            id: rightVolumePressure

                            meterModel: meterModel

                            showOverload: false
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

                        onVolumeLevelMoved: function(level) {
                            root.volumeLevelChangeRequested(Math.round(level * 100) / 100)
                        }
                    }
                }
            }

            StyledTextLabel {
                Layout.fillWidth: true

                text: qsTrc("record", "Adjust the level of your OS configured microphone")
                horizontalAlignment: Text.AlignLeft
                wrapMode: Text.WordWrap
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: 2

                color: ui.theme.strokeColor
            }

            CheckBox {
                id: checkbox

                Layout.fillWidth: true

                text: qsTrc("record", "Enable audible input monitoring")

                checked: false

                onClicked: {
                    console.log("Input monitoring toggled: " + checked)
                }
            }
        }
    }
}
