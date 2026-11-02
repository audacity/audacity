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

    property NavigationPanel durationNavigation: NavigationPanel {
        name: "TimedRecordingDuration"
        section: root.navigationSection
        order: 3
        direction: NavigationPanel.Horizontal
        enabled: setupModel.endAfterDuration
        accessible.name: qsTrc("record", "Duration")
    }

    Component.onCompleted: {
        setupModel.init()
    }

    onNavigationActivateRequested: {
        startButton.navigation.requestActive()
    }

    component SchedulePanel: Rectangle {
        id: panel

        property string title: ""
        default property alias panelData: panelContent.data

        property NavigationPanel navigation: NavigationPanel {
            name: panel.title
            section: root.navigationSection
            direction: NavigationPanel.Vertical
            enabled: panel.enabled && panel.visible
            accessible.name: panel.title
        }

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

        property NavigationPanel navigationPanel: null
        property int navigationOrder: 0

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

            navigation.panel: input.navigationPanel
            navigation.order: input.navigationOrder
            accessible.name: input.label

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

        property NavigationPanel navigationPanel: null
        property int navigationOrderStart: 0

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

            navigationPanel: moment.navigationPanel
            navigationOrder: moment.navigationOrderStart

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

            navigationPanel: moment.navigationPanel
            navigationOrder: moment.navigationOrderStart + 2

            onEdited: function (newText) {
                moment.timeEdited(newText)
            }
        }
    }

    component CompletionAction: ColumnLayout {
        id: action

        property string text: ""
        property bool checked: false
        property string path: ""

        property NavigationPanel navigationPanel: null
        property int navigationOrderStart: 0

        signal toggled(bool checked)
        signal pathEdited(string path)
        signal browseRequested

        Layout.fillWidth: true

        spacing: 8

        CheckBox {
            text: action.text
            checked: action.checked

            navigation.panel: action.navigationPanel
            navigation.order: action.navigationOrderStart

            onClicked: {
                action.toggled(!checked)
            }
        }

        RowLayout {
            Layout.fillWidth: true

            spacing: 4
            enabled: action.checked

            TextInputField {
                Layout.fillWidth: true
                Layout.preferredHeight: 28

                textSidePadding: 6
                currentText: action.path

                navigation.panel: action.navigationPanel
                navigation.order: action.navigationOrderStart + 1
                accessible.name: action.text

                onTextEdited: function (newTextValue) {
                    action.pathEdited(newTextValue)
                }
            }

            FlatButton {
                Layout.preferredHeight: 28

                icon: IconCode.OPEN_FILE
                text: qsTrc("ui", "Browse")
                buttonType: FlatButton.Horizontal
                orientation: Qt.Horizontal

                navigation.panel: action.navigationPanel
                navigation.order: action.navigationOrderStart + 3

                onClicked: {
                    action.browseRequested()
                }
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
                    id: startPanel

                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    Layout.preferredWidth: 1

                    title: qsTrc("record", "Start recording")
                    navigation.order: 1

                    RowLayout {
                        spacing: 16

                        RoundedRadioButton {
                            text: qsTrc("record", "Now")
                            checked: setupModel.startNow

                            navigation.panel: startPanel.navigation
                            navigation.order: 0

                            onToggled: {
                                setupModel.startNow = true
                            }
                        }

                        RoundedRadioButton {
                            text: qsTrc("record", "At a time")
                            checked: !setupModel.startNow

                            navigation.panel: startPanel.navigation
                            navigation.order: 1

                            onToggled: {
                                setupModel.startNow = false
                            }
                        }
                    }

                    ScheduledMoment {
                        visible: !setupModel.startNow

                        navigationPanel: startPanel.navigation
                        navigationOrderStart: 2

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
                    id: endPanel

                    Layout.fillWidth: true
                    Layout.fillHeight: true
                    Layout.preferredWidth: 1

                    title: qsTrc("record", "End recording")
                    navigation.order: 2

                    RowLayout {
                        spacing: 16

                        RoundedRadioButton {
                            text: qsTrc("record", "For a duration")
                            checked: setupModel.endAfterDuration

                            navigation.panel: endPanel.navigation
                            navigation.order: 0

                            onToggled: {
                                setupModel.endAfterDuration = true
                            }
                        }

                        RoundedRadioButton {
                            text: qsTrc("record", "At a time")
                            checked: !setupModel.endAfterDuration

                            navigation.panel: endPanel.navigation
                            navigation.order: 1

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

                            navigation.panel: root.durationNavigation
                            navigation.order: 0
                            accessibleName: qsTrc("record", "Duration") + " "

                            onValueChangeRequested: function (newValue) {
                                setupModel.durationSeconds = newValue
                            }
                        }
                    }

                    ScheduledMoment {
                        visible: !setupModel.endAfterDuration

                        navigationPanel: endPanel.navigation
                        navigationOrderStart: 2

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
                id: completionPanel

                Layout.fillWidth: true

                title: qsTrc("record", "When recording is done")
                navigation.order: 4

                CompletionAction {
                    Layout.topMargin: 8

                    text: qsTrc("record", "Save project")
                    checked: setupModel.saveProjectEnabled
                    path: setupModel.saveProjectPath

                    navigationPanel: completionPanel.navigation
                    navigationOrderStart: 0

                    onToggled: function (checked) {
                        setupModel.saveProjectEnabled = checked
                    }

                    onPathEdited: function (path) {
                        setupModel.saveProjectPath = path
                    }

                    onBrowseRequested: {
                        setupModel.browseSaveProjectPath()
                    }
                }

                CompletionAction {
                    text: qsTrc("record", "Export audio")
                    checked: setupModel.exportAudioEnabled
                    path: setupModel.exportAudioPath

                    navigationPanel: completionPanel.navigation
                    navigationOrderStart: 4

                    onToggled: function (checked) {
                        setupModel.exportAudioEnabled = checked
                    }

                    onPathEdited: function (path) {
                        setupModel.exportAudioPath = path
                    }

                    onBrowseRequested: {
                        setupModel.browseExportAudioPath()
                    }
                }

                SeparatorLine {}

                CheckBox {
                    text: qsTrc("record", "Quit Audacity")
                    enabled: setupModel.saveProjectEnabled || setupModel.exportAudioEnabled
                    checked: setupModel.quitAudacityEnabled

                    navigation.panel: completionPanel.navigation
                    navigation.order: 8

                    onClicked: {
                        setupModel.quitAudacityEnabled = !checked
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
            navigationPanel.order: 5

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
                id: startButton

                text: qsTrc("record", "Start timed recording")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.CustomButton
                accentButton: true
                isNarrow: true
                enabled: setupModel.canStart
                toolTipDescription: setupModel.summary

                onClicked: {
                    if (!setupModel.confirmOverwrite()) {
                        return
                    }

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
