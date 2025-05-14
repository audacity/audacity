/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15
import QtQuick.Controls 2.15

import Muse.UiComponents 1.0
import Muse.Ui 1.0

StyledPopupView {
    id: root

    contentWidth: 292
    contentHeight: 248

    margins: 12

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
                                    checked: true
                                    text: qsTr("Default")
                                }

                                RoundedRadioButton {
                                    text: qsTr("RMS")
                                }

                                RoundedRadioButton {
                                    text: qsTr("Gradient")
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
                                    checked: true
                                    text: qsTr("dB")
                                }

                                RoundedRadioButton {
                                    text: qsTr("Linear")
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
                                checked: true
                                text: qsTr("Top bar (horizontal)")
                            }

                            RoundedRadioButton {
                                text: qsTr("Side bar (vertical)")
                            }
                        }
                    }
                }
            }
        }
    }
}