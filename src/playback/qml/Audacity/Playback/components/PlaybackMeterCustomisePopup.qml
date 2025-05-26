/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15
import QtQuick.Controls 2.15

import Muse.UiComponents 1.0
import Muse.Ui 1.0

import Audacity.Playback 1.0

StyledPopupView {
    id: root

    property int meterStyle: PlaybackMeterStyle.Default
    property int meterType: PlaybackMeterType.DbLog
    property int meterPosition: PlaybackMeterPosition.TopBar

    signal positionChangeRequested(int position)
    signal styleChangeRequested(int style)
    signal typeChangeRequested(int type)

    contentWidth: 336
    contentHeight: 248

    margins: 12

    onMeterStyleChanged: {
        if (root.meterStyle == PlaybackMeterStyle.Default) {
            meterStyleDefault.checked = true
        } else if (root.meterStyle == PlaybackMeterStyle.RMS) {
            meterStyleRMS.checked = true
        } else if (root.meterStyle == PlaybackMeterStyle.Gradient) {
            meterStylePeak.checked = true
        }
    }

    onMeterTypeChanged: {
        if (root.meterType == PlaybackMeterType.DbLog) {
            meterTypeDbLog.checked = true
        } else if (root.meterType == PlaybackMeterType.DbLinear) {
            meterTypeDbLinear.checked = true
        } else if (root.meterType == PlaybackMeterType.Linear) {
            meterTypeLinear.checked = true
        }
    }

    onMeterPositionChanged: {
        if (root.meterPosition == PlaybackMeterPosition.TopBar) {
            meterPositionTopBar.checked = true
        } else if (root.meterPosition == PlaybackMeterPosition.SideBar) {
            meterPositionSideBar.checked = true
        }
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: 12

        RowLayout {
            Layout.fillWidth: true
            Layout.preferredHeight: 124
            
            spacing: 12

            ColumnLayout {
                Layout.fillWidth: true
                Layout.fillHeight: true

                spacing: 12

                StyledTextLabel {
                    Layout.fillWidth: true

                    text: qsTr("Meter style")
                    horizontalAlignment: Text.AlignLeft
                }
                
                Rectangle {
                    Layout.fillWidth: true
                    Layout.fillHeight: true

                    color: ui.theme.backgroundPrimaryColor
                    border.width: 1
                    border.color: ui.theme.strokeColor
                    radius: 2

                    Item {
                        anchors.fill: parent
                        anchors.margins: 12

                        RadioButtonGroup {
                            id: meterStyleGroup

                            width: parent.width

                            orientation: Qt.Vertical

                            Column {
                                id: meterStyleColumn

                                width: parent.width
                                spacing: 8

                                RoundedRadioButton {
                                    id: meterStyleDefault
                                    checked: true
                                    text: qsTr("Default")

                                    onToggled: {
                                        styleChangeRequested(PlaybackMeterStyle.Default)
                                    }
                                }

                                RoundedRadioButton {
                                    id: meterStyleRMS
                                    text: qsTr("RMS")

                                    onToggled: {
                                        styleChangeRequested(PlaybackMeterStyle.RMS)
                                    }
                                }

                                RoundedRadioButton {
                                    id: meterStylePeak
                                    text: qsTr("Gradient")

                                    onToggled: {
                                        styleChangeRequested(PlaybackMeterStyle.Gradient)
                                    }
                                }
                            }
                        }
                    }
                }
            }

            ColumnLayout {
                Layout.fillWidth: true
                Layout.fillHeight: true

                spacing: 12

                StyledTextLabel {
                    Layout.fillWidth: true

                    text: qsTr("Meter type")
                    horizontalAlignment: Text.AlignLeft
                }
                
                Rectangle {
                    Layout.fillWidth: true
                    Layout.fillHeight: true

                    color: ui.theme.backgroundPrimaryColor
                    border.width: 1
                    border.color: ui.theme.strokeColor
                    radius: 2

                    Item {
                        anchors.fill: parent
                        anchors.margins: 12

                        RadioButtonGroup {
                            id: meterTypeGroup

                            width: parent.width
                            height: parent.height

                            orientation: Qt.Vertical

                            Column {
                                id: meterTypeColumn

                                width: parent.width
                                spacing: 8

                                RoundedRadioButton {
                                    id: meterTypeDbLog

                                    //TODO: enable when dB log is implemented
                                    enabled: false
                                    text: qsTr("Logarithmic (dB)")

                                    onToggled: {
                                        typeChangeRequested(PlaybackMeterType.DbLog)
                                    }
                                }

                                RoundedRadioButton {
                                    id: meterTypeDbLinear

                                    checked: true
                                    text: qsTr("Linear (dB)")

                                    onToggled: {
                                        typeChangeRequested(PlaybackMeterType.DbLinear)
                                    }
                                }

                                RoundedRadioButton {
                                    id: meterTypeLinear

                                    //TODO: enable when linear is implemented
                                    enabled: false
                                    text: qsTr("Linear (amp)")

                                    onToggled: {
                                        typeChangeRequested(PlaybackMeterType.Linear)
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        ColumnLayout {
            Layout.fillWidth: true
            Layout.preferredHeight: 100

            spacing: 12

            StyledTextLabel {
                Layout.fillWidth: true

                text: qsTr("Position")
                horizontalAlignment: Text.AlignLeft
            }
            
            Rectangle {
                Layout.fillWidth: true
                Layout.fillHeight: true

                color: ui.theme.backgroundPrimaryColor
                border.width: 1
                border.color: ui.theme.strokeColor
                radius: 2

                Item {
                    anchors.fill: parent
                    anchors.margins: 12

                    RadioButtonGroup {
                        id: meterPositionGroup

                        width: parent.width
                        height: parent.height

                        orientation: Qt.Vertical

                        Column {
                            id: meterPositionColumn

                            width: parent.width
                            spacing: 8

                            RoundedRadioButton {
                                id: meterPositionTopBar

                                checked: true
                                text: qsTr("Top bar (horizontal)")

                                onToggled: {
                                    positionChangeRequested(PlaybackMeterPosition.TopBar)
                                }
                            }

                            RoundedRadioButton {
                                id: meterPositionSideBar

                                text: qsTr("Side bar (vertical)")

                                onToggled: {
                                    positionChangeRequested(PlaybackMeterPosition.SideBar)
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}