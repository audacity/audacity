import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectRecovery

StyledDialogView {
    id: root

    property string projectPath: ""

    title: qsTrc("projectrecovery", "Project recovery")

    contentWidth: 480
    contentHeight: content.implicitHeight
    margins: 0

    modal: true

    readonly property int stage: wizard.stage
    readonly property bool busy: stage === ProjectRecoveryWizardModel.Analyzing || stage === ProjectRecoveryWizardModel.Recovering

    ProjectRecoveryWizardModel {
        id: wizard
    }

    Component.onCompleted: {
        wizard.projectPath = root.projectPath
    }

    QtObject {
        id: prv

        readonly property int margins: 16
        readonly property int bodyBottomMargin: 24
        readonly property int footerMargins: 12
        readonly property int busyBodyHeight: 272
        readonly property int containerRadius: 4

        property bool showDetails: false

        readonly property var methods: [
            {
                method: ProjectRecoveryWizardModel.Lossless,
                title: qsTrc("projectrecovery", "1. Lossless recovery"),
                description: qsTrc("projectrecovery", "Best case, restores the full project with no quality loss")
            },
            {
                method: ProjectRecoveryWizardModel.Lossy,
                title: qsTrc("projectrecovery", "2. Lossy recovery"),
                description: qsTrc("projectrecovery", "Replaces missing audio with silence of the same length")
            },
            {
                method: ProjectRecoveryWizardModel.Database,
                title: qsTrc("projectrecovery", "3. Database recovery"),
                description: qsTrc("projectrecovery", "Last resort for severely corrupted files")
            }
        ]

        function methodTitle(method) {
            switch (method) {
            case ProjectRecoveryWizardModel.Lossless:
                return qsTrc("projectrecovery", "Lossless recovery")
            case ProjectRecoveryWizardModel.Lossy:
                return qsTrc("projectrecovery", "Lossy recovery")
            case ProjectRecoveryWizardModel.Database:
                return qsTrc("projectrecovery", "Database recovery")
            }
            return ""
        }

        function integrityText(integrity) {
            switch (integrity) {
            case ProjectRecoveryWizardModel.Passed:
                return qsTrc("projectrecovery", "PASSED")
            case ProjectRecoveryWizardModel.Failed:
                return qsTrc("projectrecovery", "FAILED")
            }
            return qsTrc("projectrecovery", "NOT CHECKED")
        }
    }

    component PageHeading: ColumnLayout {
        property string title
        property string description

        Layout.fillWidth: true
        spacing: 4

        StyledTextLabel {
            Layout.fillWidth: true
            text: parent.title
            font: ui.theme.largeBodyBoldFont
            horizontalAlignment: Text.AlignLeft
            wrapMode: Text.Wrap
        }

        StyledTextLabel {
            Layout.fillWidth: true
            visible: text !== ""
            text: parent.description
            textFormat: Text.PlainText
            horizontalAlignment: Text.AlignLeft
            wrapMode: Text.Wrap
        }
    }

    component Container: Rectangle {
        Layout.fillWidth: true
        radius: 4
        color: ui.theme.backgroundTertiaryColor
        border.color: ui.theme.strokeColor
        border.width: 1
    }

    component ValueRow: RowLayout {
        property string label
        property string value
        property bool boldValue: false

        Layout.fillWidth: true
        spacing: 8

        StyledTextLabel {
            text: parent.label
            horizontalAlignment: Text.AlignLeft
        }

        StyledTextLabel {
            Layout.fillWidth: !parent.boldValue
            text: parent.value
            textFormat: Text.PlainText
            font: parent.boldValue ? ui.theme.bodyBoldFont : ui.theme.bodyFont
            horizontalAlignment: parent.boldValue ? Text.AlignLeft : Text.AlignRight
        }
    }

    component MissingBlockList: StyledListView {
        id: missingList

        property var blocks: []

        Layout.fillWidth: true
        Layout.preferredHeight: Math.min(contentHeight, 160)
        spacing: 4
        model: missingList.blocks

        delegate: StyledTextLabel {
            required property var modelData

            width: ListView.view.width
            readonly property var values: [modelData.track, modelData.clip, modelData.start.toLocaleString(Qt.locale(), "f", 1), modelData.end.toLocaleString(Qt.locale(), "f", 1)]

            // One pass, so a name from the file that contains "%2" is not replaced again
            //: A missing piece of audio: the track name, the clip name, then its start and end in seconds
            text: qsTrc("projectrecovery", "“%1”, clip “%2”: %3 s to %4 s").replace(/%([1-4])/g, function (match, number) {
                return values[number - 1]
            })
            textFormat: Text.PlainText
            horizontalAlignment: Text.AlignLeft
            wrapMode: Text.Wrap
        }
    }

    ColumnLayout {
        id: content

        anchors.left: parent.left
        anchors.right: parent.right
        spacing: 0

        StepIndicator {
            Layout.fillWidth: true
            Layout.leftMargin: prv.margins
            Layout.rightMargin: prv.margins
            Layout.topMargin: 12
            Layout.bottomMargin: 16
            visible: !root.busy
            count: wizard.stepCount
            current: wizard.step
        }

        SeparatorLine {
            visible: !root.busy
        }

        Loader {
            Layout.fillWidth: true
            sourceComponent: {
                switch (root.stage) {
                case ProjectRecoveryWizardModel.SelectProject:
                    return selectPage
                case ProjectRecoveryWizardModel.Results:
                    return resultsPage
                case ProjectRecoveryWizardModel.ChooseMethod:
                    return methodsPage
                case ProjectRecoveryWizardModel.MethodDetails:
                    return methodPage
                case ProjectRecoveryWizardModel.Finished:
                    return finishedPage
                }
                return busyPage
            }
        }

        SeparatorLine {
            visible: !root.busy
        }

        ButtonBox {
            Layout.fillWidth: true
            Layout.margins: prv.footerMargins
            visible: !root.busy

            navigationPanel.section: root.navigationSection

            FlatButton {
                visible: root.stage === ProjectRecoveryWizardModel.Finished
                text: qsTrc("projectrecovery", "Send diagnostics report")
                buttonRole: ButtonBoxModel.CustomRole
                buttonId: ButtonBoxModel.CustomButton + 1
                isLeftSide: true
                enabled: false
            }

            FlatButton {
                visible: root.stage === ProjectRecoveryWizardModel.Results || root.stage === ProjectRecoveryWizardModel.ChooseMethod || root.stage === ProjectRecoveryWizardModel.MethodDetails
                text: qsTrc("global", "Back")
                buttonRole: ButtonBoxModel.CustomRole
                buttonId: ButtonBoxModel.CustomButton + 2
                minWidth: 80

                onClicked: wizard.back()
            }

            FlatButton {
                visible: root.stage === ProjectRecoveryWizardModel.SelectProject || root.stage === ProjectRecoveryWizardModel.ChooseMethod
                text: qsTrc("global", "Cancel")
                buttonRole: ButtonBoxModel.RejectRole
                buttonId: ButtonBoxModel.Cancel
                minWidth: 80

                onClicked: root.reject()
            }

            FlatButton {
                visible: root.stage === ProjectRecoveryWizardModel.Finished
                text: qsTrc("global", "Close")
                buttonRole: ButtonBoxModel.RejectRole
                buttonId: ButtonBoxModel.Close
                minWidth: 80

                onClicked: root.reject()
            }

            FlatButton {
                visible: root.stage === ProjectRecoveryWizardModel.SelectProject || root.stage === ProjectRecoveryWizardModel.Results
                text: qsTrc("global", "Next")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.CustomButton + 3
                minWidth: 80
                accentButton: true
                enabled: wizard.canGoNext

                onClicked: wizard.next()
            }

            FlatButton {
                visible: root.stage === ProjectRecoveryWizardModel.MethodDetails
                text: qsTrc("projectrecovery", "Run lossy recovery")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.CustomButton + 4
                accentButton: true

                onClicked: wizard.runRecovery()
            }

            FlatButton {
                visible: root.stage === ProjectRecoveryWizardModel.Finished
                text: qsTrc("projectrecovery", "Open project")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.CustomButton + 5
                accentButton: true
                enabled: wizard.errorText === ""

                onClicked: {
                    wizard.openResult()
                    root.accept()
                }
            }
        }
    }

    Component {
        id: selectPage

        Item {
            implicitHeight: selectColumn.implicitHeight + prv.margins + prv.bodyBottomMargin

            ColumnLayout {
                id: selectColumn

                anchors.fill: parent
                anchors.leftMargin: prv.margins
                anchors.rightMargin: prv.margins
                anchors.topMargin: prv.margins
                anchors.bottomMargin: prv.bodyBottomMargin
                spacing: 0

                PageHeading {
                    title: qsTrc("projectrecovery", "Select a project to recover")
                    description: qsTrc("projectrecovery", "Choose an Audacity project file (.aup4) and we’ll analyze it to help you understand what recovery options are available.")
                }

                StyledTextLabel {
                    Layout.fillWidth: true
                    Layout.topMargin: 24
                    text: qsTrc("projectrecovery", "Project file path")
                    horizontalAlignment: Text.AlignLeft
                }

                FilePicker {
                    Layout.fillWidth: true
                    Layout.topMargin: 4
                    spacing: 8
                    pickerType: FilePicker.PickerType.File
                    buttonType: FlatButton.TextOnly
                    buttonText: qsTrc("projectrecovery", "Browse")
                    pathFieldTitle: qsTrc("projectrecovery", "Project file path")
                    dialogTitle: qsTrc("projectrecovery", "Select a project to recover")
                    filter: [qsTrc("projectrecovery", "Audacity projects") + " (*.aup4 *.aup3 *.aup4unsaved)"]
                    dir: wizard.projectsDir
                    path: wizard.projectPath

                    onPathEdited: function (newPath) {
                        wizard.projectPath = newPath
                    }
                }

                StyledTextLabel {
                    Layout.fillWidth: true
                    Layout.topMargin: 8
                    visible: wizard.projectOpen
                    text: qsTrc("projectrecovery", "This project is open in Audacity. Close it, then try again.")
                    horizontalAlignment: Text.AlignLeft
                    wrapMode: Text.Wrap
                }
            }
        }
    }

    Component {
        id: busyPage

        Item {
            implicitHeight: prv.busyBodyHeight

            ColumnLayout {
                anchors.centerIn: parent
                width: parent.width - 2 * prv.margins
                spacing: 0

                readonly property bool recovering: root.stage === ProjectRecoveryWizardModel.Recovering

                StyledIconLabel {
                    Layout.alignment: Qt.AlignHCenter
                    iconCode: parent.recovering ? IconCode.CONFIGURE : IconCode.SEARCH
                    font.pixelSize: 24
                    color: ui.theme.accentColor
                }

                StyledTextLabel {
                    Layout.fillWidth: true
                    Layout.topMargin: 16
                    text: parent.recovering ? qsTrc("projectrecovery", "Running project recovery") : qsTrc("projectrecovery", "Analyzing your project")
                    font: ui.theme.tabBoldFont
                    horizontalAlignment: Text.AlignHCenter
                }

                StyledTextLabel {
                    Layout.fillWidth: true
                    Layout.topMargin: 8
                    text: parent.recovering ? qsTrc("projectrecovery", "Replacing %n missing block(s) with silence…", "", wizard.missingBlockCount) : qsTrc("projectrecovery", "Checking database integrity and scanning blocks…")
                    horizontalAlignment: Text.AlignHCenter
                    wrapMode: Text.Wrap
                }

                IndeterminateProgressBar {
                    Layout.alignment: Qt.AlignHCenter
                    Layout.topMargin: 16
                }
            }
        }
    }

    Component {
        id: resultsPage

        Item {
            implicitHeight: resultsColumn.implicitHeight + prv.margins + prv.bodyBottomMargin

            ColumnLayout {
                id: resultsColumn

                anchors.fill: parent
                anchors.leftMargin: prv.margins
                anchors.rightMargin: prv.margins
                anchors.topMargin: prv.margins
                anchors.bottomMargin: prv.bodyBottomMargin
                spacing: 16

                PageHeading {
                    title: wizard.errorText === "" ? qsTrc("projectrecovery", "Analysis complete") : qsTrc("projectrecovery", "Analysis failed")
                    description: wizard.errorText === "" ? qsTrc("projectrecovery", "Your project has been analyzed. Review the issues below and proceed to recovery.") : wizard.errorText
                }

                Container {
                    visible: wizard.errorText === ""
                    implicitHeight: projectColumn.implicitHeight + 2 * prv.margins

                    ColumnLayout {
                        id: projectColumn

                        anchors.fill: parent
                        anchors.margins: prv.margins
                        spacing: 8

                        StyledTextLabel {
                            Layout.fillWidth: true
                            text: wizard.fileName
                            textFormat: Text.PlainText
                            font: ui.theme.bodyBoldFont
                            horizontalAlignment: Text.AlignLeft
                            elide: Text.ElideRight
                        }

                        StyledTextLabel {
                            Layout.fillWidth: true
                            text: wizard.projectPath
                            textFormat: Text.PlainText
                            horizontalAlignment: Text.AlignLeft
                            elide: Text.ElideMiddle
                        }

                        StyledTextLabel {
                            Layout.fillWidth: true
                            text: {
                                let facts = []
                                if (wizard.hasDocument) {
                                    facts.push(qsTrc("projectrecovery", "%n Track(s)", "", wizard.trackCount))
                                    facts.push(qsTrc("projectrecovery", "%n Clip(s)", "", wizard.clipCount))
                                }
                                facts.push(qsTrc("projectrecovery", "%1 MB").arg(Math.round(wizard.fileSize / (1024 * 1024))))
                                if (wizard.sampleRate > 0) {
                                    facts.push(qsTrc("projectrecovery", "%1 Hz").arg(Number(wizard.sampleRate).toLocaleString(Qt.locale(), "f", 0)))
                                }
                                return facts.join(" | ")
                            }
                            textFormat: Text.PlainText
                            horizontalAlignment: Text.AlignLeft
                        }

                        SeparatorLine {}

                        Rectangle {
                            Layout.fillWidth: true
                            implicitHeight: detailsColumn.implicitHeight + 2 * 12
                            visible: prv.showDetails
                            radius: prv.containerRadius
                            color: ui.theme.backgroundPrimaryColor
                            border.color: ui.theme.strokeColor
                            border.width: 1

                            ColumnLayout {
                                id: detailsColumn

                                anchors.fill: parent
                                anchors.margins: 12
                                spacing: 8

                                StyledTabBar {
                                    id: detailsTabs

                                    Layout.fillWidth: true

                                    StyledTabButton {
                                        text: qsTrc("projectrecovery", "Summary")
                                    }

                                    StyledTabButton {
                                        text: qsTrc("projectrecovery", "Missing blocks")
                                    }
                                }

                                ColumnLayout {
                                    Layout.fillWidth: true
                                    visible: detailsTabs.currentIndex === 0
                                    spacing: 4

                                    Repeater {
                                        model: {
                                            let rows = [
                                                {
                                                    heading: qsTrc("projectrecovery", "Database status")
                                                },
                                                {
                                                    label: qsTrc("projectrecovery", "Integrity check:"),
                                                    value: prv.integrityText(wizard.integrity)
                                                },
                                                {
                                                    heading: qsTrc("projectrecovery", "Project state")
                                                },
                                                {
                                                    label: qsTrc("projectrecovery", "Autosave data:"),
                                                    value: wizard.hasAutosave ? qsTrc("projectrecovery", "Present") : qsTrc("projectrecovery", "None")
                                                },
                                                {
                                                    label: qsTrc("projectrecovery", "Missing blocks:"),
                                                    value: wizard.missingBlockCount
                                                },
                                                {
                                                    label: qsTrc("projectrecovery", "Orphan blocks:"),
                                                    value: wizard.orphanBlockCount
                                                },
                                                {
                                                    heading: qsTrc("projectrecovery", "Statistics")
                                                }
                                            ]
                                            if (wizard.hasDocument) {
                                                rows.push({
                                                    label: qsTrc("projectrecovery", "Project version:"),
                                                    value: wizard.projectVersion
                                                })
                                                rows.push({
                                                    label: qsTrc("projectrecovery", "Sample rate:"),
                                                    value: qsTrc("projectrecovery", "%1 Hz").arg(Number(wizard.sampleRate).toLocaleString(Qt.locale(), "f", 0))
                                                })
                                                rows.push({
                                                    label: qsTrc("projectrecovery", "Tracks:"),
                                                    value: wizard.trackCount
                                                })
                                                rows.push({
                                                    label: qsTrc("projectrecovery", "Clips:"),
                                                    value: wizard.clipCount
                                                })
                                            }
                                            rows.push({
                                                label: qsTrc("projectrecovery", "Sample blocks:"),
                                                value: wizard.blockRowCount
                                            })
                                            return rows
                                        }

                                        ValueRow {
                                            required property var modelData
                                            readonly property bool isHeading: modelData.heading !== undefined

                                            label: isHeading ? "--- " + modelData.heading + " ---" : modelData.label
                                            value: isHeading ? "" : modelData.value
                                            boldValue: true
                                        }
                                    }
                                }

                                MissingBlockList {
                                    visible: detailsTabs.currentIndex === 1
                                    blocks: wizard.missingBlocks
                                }
                            }
                        }

                        RowLayout {
                            Layout.fillWidth: true
                            spacing: 8

                            FlatButton {
                                transparent: true
                                orientation: Qt.Horizontal
                                icon: prv.showDetails ? IconCode.UP : IconCode.DOWN
                                text: prv.showDetails ? qsTrc("projectrecovery", "Hide advanced details") : qsTrc("projectrecovery", "Show advanced details")

                                onClicked: prv.showDetails = !prv.showDetails
                            }

                            Item {
                                Layout.fillWidth: true
                            }

                            FlatButton {
                                enabled: false
                                text: qsTrc("projectrecovery", "Advanced tools")
                            }
                        }
                    }
                }
            }
        }
    }

    Component {
        id: methodsPage

        Item {
            implicitHeight: methodsColumn.implicitHeight + prv.margins + prv.bodyBottomMargin

            ColumnLayout {
                id: methodsColumn

                anchors.fill: parent
                anchors.leftMargin: prv.margins
                anchors.rightMargin: prv.margins
                anchors.topMargin: prv.margins
                anchors.bottomMargin: prv.bodyBottomMargin
                spacing: 16

                PageHeading {
                    title: qsTrc("projectrecovery", "Choose a recovery method")
                    description: qsTrc("projectrecovery", "These may not always work, they’re things you can try. Start with the first method, and if it doesn’t fully recover your project, return here and try the next.")
                }

                Callout {
                    Layout.fillWidth: true
                    type: Callout.Type.Info
                    text: qsTrc("projectrecovery", "Recovery writes a new file. Your original file remains untouched, so it’s safe to try different methods.")
                }

                NavigationPanel {
                    id: methodsNavigationPanel

                    name: "RecoveryMethods"
                    section: root.navigationSection
                    direction: NavigationPanel.Vertical
                    order: 1
                }

                ColumnLayout {
                    Layout.fillWidth: true
                    spacing: 8

                    Repeater {
                        model: prv.methods

                        FocusableControl {
                            id: methodItem

                            required property var modelData
                            required property int index

                            Layout.fillWidth: true
                            implicitHeight: methodRow.implicitHeight + 2 * 12
                            enabled: wizard.availableMethods.indexOf(modelData.method) !== -1
                            opacity: enabled ? 1 : ui.theme.itemOpacityDisabled

                            background.radius: 4
                            background.color: ui.theme.backgroundTertiaryColor
                            background.border.color: ui.theme.strokeColor
                            background.border.width: 1

                            navigation.name: methodItem.modelData.title
                            navigation.panel: methodsNavigationPanel
                            navigation.row: methodItem.index
                            navigation.accessible.role: MUAccessible.Button
                            navigation.accessible.name: methodItem.modelData.title + ". " + methodItem.modelData.description

                            mouseArea.enabled: methodItem.enabled
                            mouseArea.cursorShape: Qt.PointingHandCursor
                            mouseArea.onClicked: wizard.selectMethod(methodItem.modelData.method)

                            onNavigationTriggered: wizard.selectMethod(methodItem.modelData.method)

                            RowLayout {
                                id: methodRow

                                anchors.fill: parent
                                anchors.margins: 12
                                spacing: 12

                                ColumnLayout {
                                    Layout.fillWidth: true
                                    spacing: 4

                                    StyledTextLabel {
                                        Layout.fillWidth: true
                                        text: methodItem.modelData.title
                                        font: ui.theme.bodyBoldFont
                                        horizontalAlignment: Text.AlignLeft
                                    }

                                    StyledTextLabel {
                                        Layout.fillWidth: true
                                        text: methodItem.modelData.description
                                        horizontalAlignment: Text.AlignLeft
                                        wrapMode: Text.Wrap
                                    }
                                }

                                StyledIconLabel {
                                    iconCode: IconCode.CHEVRON_RIGHT
                                    font.pixelSize: 16
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    Component {
        id: methodPage

        Item {
            implicitHeight: methodColumn.implicitHeight + prv.margins + prv.bodyBottomMargin

            ColumnLayout {
                id: methodColumn

                anchors.fill: parent
                anchors.leftMargin: prv.margins
                anchors.rightMargin: prv.margins
                anchors.topMargin: prv.margins
                anchors.bottomMargin: prv.bodyBottomMargin
                spacing: 16

                PageHeading {
                    title: prv.methodTitle(wizard.method)
                    description: qsTrc("projectrecovery", "Missing audio is replaced with silence of the same length. All other audio stays as it is.")
                }

                Container {
                    implicitHeight: foundColumn.implicitHeight + 12 + 16

                    ColumnLayout {
                        id: foundColumn

                        anchors.fill: parent
                        anchors.leftMargin: 12
                        anchors.rightMargin: 12
                        anchors.topMargin: 12
                        anchors.bottomMargin: 16
                        spacing: 8

                        StyledTextLabel {
                            Layout.fillWidth: true
                            text: qsTrc("projectrecovery", "What we found in your project")
                            font: ui.theme.bodyBoldFont
                            horizontalAlignment: Text.AlignLeft
                        }

                        Repeater {
                            model: [
                                {
                                    label: qsTrc("projectrecovery", "Sample blocks"),
                                    value: wizard.blockRowCount
                                },
                                {
                                    label: qsTrc("projectrecovery", "Missing blocks"),
                                    value: wizard.missingBlockCount
                                },
                                {
                                    label: qsTrc("projectrecovery", "Orphan blocks"),
                                    value: wizard.orphanBlockCount
                                }
                            ]

                            ValueRow {
                                required property var modelData

                                label: modelData.label
                                value: modelData.value
                            }
                        }
                    }
                }
            }
        }
    }

    Component {
        id: finishedPage

        Item {
            implicitHeight: finishedColumn.implicitHeight + prv.margins + prv.bodyBottomMargin

            ColumnLayout {
                id: finishedColumn

                anchors.fill: parent
                anchors.leftMargin: prv.margins
                anchors.rightMargin: prv.margins
                anchors.topMargin: prv.margins
                anchors.bottomMargin: prv.bodyBottomMargin
                spacing: 16

                readonly property bool failed: wizard.errorText !== ""

                ColumnLayout {
                    Layout.fillWidth: true
                    Layout.topMargin: 16
                    spacing: 4

                    StyledIconLabel {
                        Layout.alignment: Qt.AlignHCenter
                        Layout.bottomMargin: 8
                        visible: !finishedColumn.failed
                        iconCode: IconCode.TICK_RIGHT_ANGLE
                        font.pixelSize: 24
                        color: ui.theme.accentColor
                    }

                    StyledTextLabel {
                        Layout.fillWidth: true
                        text: finishedColumn.failed ? qsTrc("projectrecovery", "Recovery failed") : qsTrc("projectrecovery", "Recovery attempt finished")
                        font: ui.theme.largeBodyBoldFont
                        horizontalAlignment: Text.AlignHCenter
                    }

                    StyledTextLabel {
                        Layout.fillWidth: true
                        text: finishedColumn.failed ? wizard.errorText : qsTrc("projectrecovery", "The recovery process has run. Open your project to check if it worked.")
                        textFormat: Text.PlainText
                        horizontalAlignment: Text.AlignHCenter
                        wrapMode: Text.Wrap
                    }
                }

                Container {
                    implicitHeight: resultColumn.implicitHeight + 2 * prv.margins

                    ColumnLayout {
                        id: resultColumn

                        anchors.fill: parent
                        anchors.margins: prv.margins
                        spacing: 4

                        StyledTextLabel {
                            Layout.fillWidth: true
                            text: qsTrc("projectrecovery", "Method used")
                            horizontalAlignment: Text.AlignLeft
                        }

                        StyledTextLabel {
                            Layout.fillWidth: true
                            text: prv.methodTitle(wizard.method)
                            font: ui.theme.bodyBoldFont
                            horizontalAlignment: Text.AlignLeft
                        }

                        StyledTextLabel {
                            Layout.fillWidth: true
                            Layout.topMargin: 12
                            visible: !finishedColumn.failed
                            text: qsTrc("projectrecovery", "Recovered file")
                            horizontalAlignment: Text.AlignLeft
                        }

                        StyledTextLabel {
                            Layout.fillWidth: true
                            visible: !finishedColumn.failed
                            text: wizard.outputPath
                            textFormat: Text.PlainText
                            font: ui.theme.bodyBoldFont
                            horizontalAlignment: Text.AlignLeft
                            wrapMode: Text.WrapAnywhere
                        }
                    }
                }

                Callout {
                    Layout.fillWidth: true
                    visible: !finishedColumn.failed
                    type: Callout.Type.Info
                    text: qsTrc("projectrecovery", "Open the project to check what’s missing. If important content didn’t come back, you can try another method.")
                }
            }
        }
    }
}
