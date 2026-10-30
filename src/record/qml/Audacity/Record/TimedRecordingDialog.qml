/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.UiComponents
import Audacity.Record

StyledDialogView {
    id: root

    title: qsTrc("record", "Timed recording")

    contentWidth: 560
    contentHeight: content.implicitHeight

    TimedRecordingSetupModel {
        id: setupModel
    }

    Component.onCompleted: {
        setupModel.init()
    }

    component SchedulePanel: Rectangle {
        id: panel

        property string title: ""
        default property alias panelData: panelContent.data

        implicitHeight: panelColumn.implicitHeight + 2 * 16

        color: ui.theme.backgroundTertiaryColor
        border.color: ui.theme.strokeColor
        border.width: 1
        radius: 8

        ColumnLayout {
            id: panelColumn

            anchors.fill: parent
            anchors.margins: 16

            spacing: 8

            StyledTextLabel {
                Layout.fillWidth: true

                horizontalAlignment: Text.AlignLeft
                font: ui.theme.bodyBoldFont
                text: panel.title
            }

            ColumnLayout {
                id: panelContent

                Layout.fillWidth: true

                spacing: 16
            }

            Item {
                Layout.fillHeight: true
            }
        }
    }

    component LabeledInput: ColumnLayout {
        id: input

        property string label: ""
        property string text: ""
        property string hint: ""

        signal edited(string newText)

        spacing: 4

        StyledTextLabel {
            Layout.fillWidth: true

            horizontalAlignment: Text.AlignLeft
            text: input.label
        }

        TextInputField {
            Layout.fillWidth: true
            Layout.preferredHeight: 28

            textSidePadding: 6
            currentText: input.text
            hint: input.hint

            onTextEdited: function (newTextValue) {
                input.edited(newTextValue)
            }
        }
    }

    component ScheduledMoment: RowLayout {
        id: moment

        property string date: ""
        property string time: ""
        property string dateHint: ""

        signal dateEdited(string newDate)
        signal timeEdited(string newTime)

        Layout.fillWidth: true

        spacing: 8

        LabeledInput {
            Layout.fillWidth: true
            Layout.preferredWidth: 1

            label: qsTrc("record", "Date")
            text: moment.date
            hint: moment.dateHint

            onEdited: function (newText) {
                moment.dateEdited(newText)
            }
        }

        LabeledInput {
            Layout.fillWidth: true
            Layout.preferredWidth: 1

            label: qsTrc("record", "Time")
            text: moment.time
            hint: "hh:mm:ss"

            onEdited: function (newText) {
                moment.timeEdited(newText)
            }
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

            spacing: 16

            RowLayout {
                Layout.fillWidth: true

                spacing: 16

                SchedulePanel {
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    Layout.preferredWidth: 1

                    title: qsTrc("record", "Start recording")

                    RowLayout {
                        spacing: 16

                        RoundedRadioButton {
                            text: qsTrc("record", "Now")
                            checked: setupModel.startNow

                            onToggled: {
                                setupModel.startNow = true
                            }
                        }

                        RoundedRadioButton {
                            text: qsTrc("record", "At a time")
                            checked: !setupModel.startNow

                            onToggled: {
                                setupModel.startNow = false
                            }
                        }
                    }

                    ScheduledMoment {
                        visible: !setupModel.startNow

                        date: setupModel.startDate
                        time: setupModel.startTime
                        dateHint: setupModel.dateFormat.toLowerCase()

                        onDateEdited: function (newDate) {
                            setupModel.startDate = newDate
                        }

                        onTimeEdited: function (newTime) {
                            setupModel.startTime = newTime
                        }
                    }
                }

                SchedulePanel {
                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    Layout.preferredWidth: 1

                    title: qsTrc("record", "End recording")

                    RowLayout {
                        spacing: 16

                        RoundedRadioButton {
                            text: qsTrc("record", "For a duration")
                            checked: setupModel.endAfterDuration

                            onToggled: {
                                setupModel.endAfterDuration = true
                            }
                        }

                        RoundedRadioButton {
                            text: qsTrc("record", "At a time")
                            checked: !setupModel.endAfterDuration

                            onToggled: {
                                setupModel.endAfterDuration = false
                            }
                        }
                    }

                    ColumnLayout {
                        visible: setupModel.endAfterDuration

                        spacing: 4

                        StyledTextLabel {
                            Layout.fillWidth: true

                            horizontalAlignment: Text.AlignLeft
                            text: qsTrc("record", "Duration")
                        }

                        Timecode {
                            Layout.preferredHeight: 28

                            value: setupModel.durationSeconds
                            mode: TimecodeModeSelector.Duration
                            currentFormat: setupModel.durationFormat
                            showMenu: false

                            onValueChangeRequested: function (newValue) {
                                setupModel.durationSeconds = newValue
                            }
                        }
                    }

                    ScheduledMoment {
                        visible: !setupModel.endAfterDuration

                        date: setupModel.endDate
                        time: setupModel.endTime
                        dateHint: setupModel.dateFormat.toLowerCase()

                        onDateEdited: function (newDate) {
                            setupModel.endDate = newDate
                        }

                        onTimeEdited: function (newTime) {
                            setupModel.endTime = newTime
                        }
                    }
                }
            }

            StyledTextLabel {
                Layout.fillWidth: true
                Layout.preferredHeight: 16

                horizontalAlignment: Text.AlignLeft
                text: setupModel.summary
            }

            SchedulePanel {
                Layout.fillWidth: true

                title: qsTrc("record", "When recording is done")

                ColumnLayout {
                    Layout.fillWidth: true
                    Layout.topMargin: 8

                    spacing: 8

                    CheckBox {
                        text: qsTrc("record", "Save project")
                        checked: setupModel.saveProjectEnabled

                        onClicked: {
                            setupModel.saveProjectEnabled = !checked
                        }
                    }

                    RowLayout {
                        Layout.fillWidth: true

                        spacing: 4
                        enabled: setupModel.saveProjectEnabled

                        TextInputField {
                            Layout.fillWidth: true
                            Layout.preferredHeight: 28

                            textSidePadding: 6
                            currentText: setupModel.saveProjectPath

                            onTextEdited: function (newTextValue) {
                                setupModel.saveProjectPath = newTextValue
                            }
                        }

                        FlatButton {
                            Layout.preferredHeight: 28

                            icon: IconCode.OPEN_FILE
                            text: qsTrc("ui", "Browse")
                            buttonType: FlatButton.Horizontal
                            orientation: Qt.Horizontal

                            onClicked: {
                                setupModel.browseSaveProjectPath()
                            }
                        }
                    }
                }
            }
        }

        SeparatorLine {}

        ButtonBox {
            id: buttonBox

            Layout.fillWidth: true
            Layout.margins: 8

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 1

            FlatButton {
                text: qsTrc("global", "Cancel")
                buttonRole: ButtonBoxModel.RejectRole
                buttonId: ButtonBoxModel.Cancel
                isNarrow: true

                onClicked: {
                    root.reject()
                }
            }

            FlatButton {
                text: qsTrc("record", "Start timed recording")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.CustomButton
                accentButton: true
                isNarrow: true
                enabled: setupModel.canStart

                onClicked: {
                    root.ret = {
                        errcode: 0,
                        value: setupModel.scheduleData()
                    }
                    root.hide()
                }
            }
        }
    }
}
