/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Preferences

StyledDialogView {
    id: root

    //! Set by the caller when the preferences page has changes that are not applied yet
    property bool pendingChanges: false

    title: qsTrc("preferences", "Measure latency")

    contentWidth: 480
    contentHeight: content.implicitHeight

    LatencyMeasurementModel {
        id: measurementModel
    }

    ColumnLayout {
        id: content

        anchors.left: parent.left
        anchors.right: parent.right

        spacing: 0

        ColumnLayout {
            Layout.fillWidth: true
            Layout.margins: 16

            spacing: 12

            StyledTextLabel {
                Layout.fillWidth: true
                horizontalAlignment: Text.AlignLeft
                wrapMode: Text.WordWrap
                text: qsTrc("preferences", "Connect an output of your audio interface to one of its inputs with a cable. Or hold the microphone close to the speaker or headphones.")
            }

            StyledTextLabel {
                Layout.fillWidth: true
                horizontalAlignment: Text.AlignLeft
                wrapMode: Text.WordWrap
                text: qsTrc("preferences", "The test plays short noise bursts for 4 seconds. Lower the volume if you use headphones.")
            }

            StyledTextLabel {
                Layout.fillWidth: true
                visible: root.pendingChanges
                horizontalAlignment: Text.AlignLeft
                wrapMode: Text.WordWrap
                color: ui.theme.extra["error_text_color"]
                text: qsTrc("preferences", "The test uses the applied audio settings. Apply your changes to devices and buffer first.")
            }

            SeparatorLine {}

            StyledTextLabel {
                Layout.fillWidth: true
                visible: measurementModel.state === LatencyMeasurementModel.Measuring
                horizontalAlignment: Text.AlignLeft
                text: qsTrc("preferences", "Measuring…")
            }

            StyledTextLabel {
                Layout.fillWidth: true
                visible: text.length > 0
                horizontalAlignment: Text.AlignLeft
                font: ui.theme.bodyBoldFont
                text: measurementModel.resultText
            }

            StyledTextLabel {
                Layout.fillWidth: true
                visible: text.length > 0
                horizontalAlignment: Text.AlignLeft
                wrapMode: Text.WordWrap
                text: measurementModel.reportedText + (measurementModel.detailsText.length > 0 ? " " + measurementModel.detailsText : "")
            }

            StyledTextLabel {
                Layout.fillWidth: true
                visible: text.length > 0
                horizontalAlignment: Text.AlignLeft
                wrapMode: Text.WordWrap
                color: ui.theme.extra["error_text_color"]
                text: measurementModel.warningText
            }
        }

        SeparatorLine {}

        ButtonBox {
            Layout.fillWidth: true
            Layout.margins: 8

            buttons: [ButtonBoxModel.Cancel]

            FlatButton {
                text: measurementModel.state === LatencyMeasurementModel.Idle ? qsTrc("preferences", "Start") : qsTrc("preferences", "Measure again")
                buttonRole: ButtonBoxModel.CustomRole
                buttonId: ButtonBoxModel.CustomButton + 1
                isLeftSide: true
                enabled: measurementModel.state !== LatencyMeasurementModel.Measuring

                onClicked: measurementModel.start()
            }

            FlatButton {
                //: Uses the measured latency as the recording latency compensation
                text: qsTrc("preferences", "Use result")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.Apply
                accentButton: true
                enabled: measurementModel.state === LatencyMeasurementModel.Measured

                onClicked: {
                    root.ret = {
                        errcode: 0,
                        value: measurementModel.compensationMs
                    }
                    root.hide()
                }
            }

            onStandardButtonClicked: function (buttonId) {
                if (buttonId === ButtonBoxModel.Cancel) {
                    root.reject()
                }
            }
        }
    }
}
