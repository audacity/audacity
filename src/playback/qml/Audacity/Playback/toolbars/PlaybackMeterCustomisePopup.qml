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

                    text: qsTrc("Playback", "Meter style")
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

                            orientation: Qt.Vertical
                            width: parent.width

                            spacing: 8

                            model: [
                                {label : "Default", style: PlaybackMeterStyle.Default},
                                {label : "RMS", style: PlaybackMeterStyle.RMS},
                                {label : "Gradient", style: PlaybackMeterStyle.Gradient}
                            ]

                            delegate: RoundedRadioButton {
                                id: meterStyleButton
                                text: qsTrc("playback", modelData["label"])
                                checked: root.meterStyle == modelData["style"]

                                onToggled: {
                                    root.styleChangeRequested(modelData["style"])
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

                    text: qsTrc("Playback", "Meter type")
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

                            orientation: Qt.Vertical

                            width: parent.width
                            height: parent.height

                            spacing: 8

                            //TODO: Modify when dbLog and Linear (amp) are implemented
                            model: [
                                {label : "Logarithmic (dB)", type: PlaybackMeterType.DbLog, enabled: false, checked: false},
                                {label : "Linear (dB)", type: PlaybackMeterType.DbLinear, enabled: true, checked: true},
                                {label : "Linear (amp)", type: PlaybackMeterType.Linear, enabled: false, checked: false},
                            ]

                            delegate: RoundedRadioButton {
                                id: meterTypeButton
                                text: qsTrc("playback", modelData["label"])
                                checked: modelData["checked"]
                                enabled: modelData["enabled"]

                                onToggled: {
                                    root.typeChangeRequested(modelData["type"])
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

                        orientation: Qt.Vertical

                        width: parent.width
                        height: parent.height

                        spacing: 8

                        model: [
                            {label : "Top bar (horizontal)", position: PlaybackMeterPosition.TopBar},
                            {label : "Side bar (vertical)", position: PlaybackMeterPosition.SideBar}
                        ]

                        delegate: RoundedRadioButton {
                            id: meterPositionButton
                            text: qsTrc("playback", modelData["label"])
                            checked: root.meterPosition == modelData["position"]

                            onToggled: {
                                root.positionChangeRequested(modelData["position"])
                            }
                        }
                    }
                }
            }
        }
    }
}