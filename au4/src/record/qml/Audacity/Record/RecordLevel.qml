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
    property alias leftRecentPeak: leftVolumePressure.recentPeak
    property alias leftMaxPeak: leftVolumePressure.maxPeak

    property alias rightCurrentVolumePressure: rightVolumePressure.currentVolumePressure
    property alias rightRecentPeak: rightVolumePressure.recentPeak
    property alias rightMaxPeak: rightVolumePressure.maxPeak

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

    StyledPopupView {
        id: popup

        margins: 12

        contentWidth: 232
        contentHeight: 108

        onOpened: {
            leftVolumePressure.requestPaint()
            rightVolumePressure.requestPaint()
        }

        ColumnLayout {
            id: content

            anchors.fill: parent

            spacing: 12

            StyledTextLabel {
                Layout.fillWidth: true

                text: qsTrc("record", "Microphone level")
                horizontalAlignment: Text.AlignLeft
            }

            Rectangle {
                Layout.fillWidth: true
                Layout.preferredHeight: 44

                color: ui.theme.backgroundPrimaryColor
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

                        VolumePressureMeter {
                            id: leftVolumePressure
                        }
                        VolumePressureMeter {
                            id: rightVolumePressure
                            showRuler: true
                        }
                    }

                    VolumeSlider {
                        id: volumeSlider

                        anchors.left: parent.left
                        anchors.leftMargin: -handleWidth/2
                        anchors.right: parent.right
                        anchors.rightMargin: -handleWidth/2 + leftVolumePressure.overloadWidth
                        anchors.top: parent.top
                        anchors.topMargin: -1

                        onVolumeLevelMoved: function(level) {
                            root.volumeLevelChangeRequested(Math.round(level * 10) / 10)
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
        }
    }
}
