/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Record

StyledDialogView {
    id: root

    title: qsTrc("record", "Timed recording")

    contentWidth: 400
    contentHeight: content.implicitHeight

    resizable: false
    closeOnEscape: false

    TimedRecordingProgressModel {
        id: progressModel
    }

    Component.onCompleted: {
        progressModel.init()
    }

    onNavigationActivateRequested: {
        stopButton.navigation.requestActive()
    }

    navigationSection.onNavigationEvent: function (event) {
        if (event.type === NavigationEvent.Escape) {
            event.accepted = true
            progressModel.stop()
        }
    }

    ColumnLayout {
        id: content

        anchors.fill: parent

        spacing: 0

        ColumnLayout {
            Layout.fillWidth: true
            Layout.topMargin: 16
            Layout.leftMargin: 16
            Layout.rightMargin: 16
            Layout.bottomMargin: 24

            spacing: 8

            Row {
                Layout.alignment: Qt.AlignHCenter

                spacing: 6

                Rectangle {
                    anchors.verticalCenter: parent.verticalCenter
                    width: 8
                    height: 8

                    visible: progressModel.isRecording
                    radius: width / 2
                    color: ui.theme.extra["record_color"]
                }

                StyledTextLabel {
                    text: progressModel.statusText
                }
            }

            StyledTextLabel {
                Layout.fillWidth: true

                horizontalAlignment: Text.AlignHCenter
                text: progressModel.clockText
                font.pixelSize: 32
                font.bold: true
            }

            StyledTextLabel {
                Layout.fillWidth: true

                visible: !progressModel.isQuitting
                horizontalAlignment: Text.AlignHCenter
                text: progressModel.startText
            }

            StyledTextLabel {
                Layout.fillWidth: true

                visible: !progressModel.isQuitting
                horizontalAlignment: Text.AlignHCenter
                text: progressModel.endText
            }
        }

        SeparatorLine {}

        ButtonBox {
            Layout.fillWidth: true
            Layout.margins: 8

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 1

            FlatButton {
                id: stopButton

                text: progressModel.isQuitting ? qsTrc("global", "Cancel") : qsTrc("record", "Stop")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.CustomButton
                accentButton: true
                isNarrow: true
                toolTipDescription: progressModel.isQuitting ? "" : progressModel.startText + ". " + progressModel.endText

                onClicked: {
                    progressModel.stop()
                }
            }
        }
    }
}
