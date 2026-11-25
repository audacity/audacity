/*
* Audacity: A Digital Audio Editor
*/
pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.UiComponents 1.0
import Audacity.TrackEdit 1.0

StyledDialogView {
    id: root

    property var stepData: ({})

    title: paramsModel.title

    contentWidth: prv.contentWidth
    contentHeight: content.implicitHeight
    margins: prv.margins

    QtObject {
        id: prv

        readonly property int contentWidth: 480
        readonly property int margins: 12
        readonly property int labelColumnWidth: 120
        readonly property int dropdownWidth: 200
        readonly property int numberWidth: 96
        readonly property int sectionsSpacing: 16
        readonly property int rowSpacing: 8
        readonly property int itemSpacing: 8
        readonly property int buttonWidth: 64
        readonly property int buttonHeight: 28
        readonly property int buttonsNavigationOrder: 1000

        function confirm() {
            root.ret = {
                errcode: 0,
                value: paramsModel.stepData()
            }
            root.hide()
        }
    }

    SelectStepParamsModel {
        id: paramsModel
    }

    Component.onCompleted: {
        paramsModel.load(root.stepData)
    }

    onNavigationActivateRequested: {
        timeKindRow.dropdown.navigation.requestActive()
    }

    component RowLabel: Item {
        id: rowLabel

        property alias text: label.text

        Layout.preferredWidth: prv.labelColumnWidth
        Layout.fillHeight: true

        StyledTextLabel {
            id: label

            anchors.verticalCenter: parent.verticalCenter
            horizontalAlignment: Text.AlignLeft
        }
    }

    component ChoiceRow: RowLayout {
        id: choiceRow

        property string title: ""
        property var options: []
        property string value: ""
        property NavigationPanel navigationPanel: null
        property int navigationOrder: 0
        property alias dropdown: dropdown

        signal choiceSelected(string value)

        spacing: prv.itemSpacing

        RowLabel {
            text: choiceRow.title
        }

        StyledDropdown {
            id: dropdown

            Layout.preferredWidth: prv.dropdownWidth

            model: choiceRow.options
            currentIndex: dropdown.indexOfValue(choiceRow.value)

            navigation.panel: choiceRow.navigationPanel
            navigation.order: choiceRow.navigationOrder
            navigation.accessible.name: choiceRow.title + ": " + dropdown.currentText

            onActivated: function (index, value) {
                choiceRow.choiceSelected(value)
            }
        }
    }

    component EdgeRow: RowLayout {
        id: edgeRow

        property string title: ""
        property var options: []
        property string anchorValue: ""
        property real offset: 0.0
        property NavigationPanel navigationPanel: null
        property int navigationOrder: 0

        signal anchorSelected(string value)
        signal offsetEdited(real value)

        spacing: prv.itemSpacing

        RowLabel {
            text: edgeRow.title
        }

        StyledDropdown {
            id: anchorDropdown

            Layout.preferredWidth: prv.dropdownWidth

            model: edgeRow.options
            currentIndex: anchorDropdown.indexOfValue(edgeRow.anchorValue)

            navigation.panel: edgeRow.navigationPanel
            navigation.order: edgeRow.navigationOrder
            navigation.accessible.name: edgeRow.title + ": " + anchorDropdown.currentText

            onActivated: function (index, value) {
                edgeRow.anchorSelected(value)
            }
        }

        IncrementalPropertyControl {
            Layout.preferredWidth: prv.numberWidth

            currentValue: edgeRow.offset
            minValue: -86400
            maxValue: 86400
            step: 0.1
            decimals: 3
            measureUnitsSymbol: "s"

            navigation.panel: edgeRow.navigationPanel
            navigation.order: edgeRow.navigationOrder + 1
            navigation.accessible.name: edgeRow.title + " " + qsTrc("trackedit", "offset")

            onValueEdited: function (newValue) {
                edgeRow.offsetEdited(newValue)
            }
        }
    }

    component NumberRow: RowLayout {
        id: numberRow

        property string title: ""
        property int value: 1
        property NavigationPanel navigationPanel: null
        property int navigationOrder: 0

        signal numberEdited(int value)

        spacing: prv.itemSpacing

        RowLabel {
            text: numberRow.title
        }

        IncrementalPropertyControl {
            Layout.preferredWidth: prv.numberWidth

            currentValue: numberRow.value
            minValue: 1
            maxValue: 999
            step: 1
            decimals: 0

            navigation.panel: numberRow.navigationPanel
            navigation.order: numberRow.navigationOrder
            navigation.accessible.name: numberRow.title

            onValueEdited: function (newValue) {
                numberRow.numberEdited(Math.round(newValue))
            }
        }
    }

    ColumnLayout {
        id: content

        width: parent.width
        spacing: prv.sectionsSpacing

        BaseSection {
            id: timeSection

            Layout.fillWidth: true

            title: qsTrc("trackedit", "Time")
            rowSpacing: prv.rowSpacing

            navigation.section: root.navigationSection
            navigation.order: 1

            ChoiceRow {
                id: timeKindRow

                title: qsTrc("trackedit", "Select")
                options: paramsModel.timeKindOptions
                value: paramsModel.timeKind
                navigationPanel: timeSection.navigation
                navigationOrder: 1

                onChoiceSelected: function (value) {
                    paramsModel.timeKind = value
                }
            }

            EdgeRow {
                visible: paramsModel.timeKind === "Range"

                title: qsTrc("trackedit", "From")
                options: paramsModel.startAnchorOptions
                anchorValue: paramsModel.startAnchor
                offset: paramsModel.startOffset
                navigationPanel: timeSection.navigation
                navigationOrder: 2

                onAnchorSelected: function (value) {
                    paramsModel.startAnchor = value
                }
                onOffsetEdited: function (value) {
                    paramsModel.startOffset = value
                }
            }

            EdgeRow {
                visible: paramsModel.timeKind === "Range"

                title: qsTrc("trackedit", "To")
                options: paramsModel.endAnchorOptions
                anchorValue: paramsModel.endAnchor
                offset: paramsModel.endOffset
                navigationPanel: timeSection.navigation
                navigationOrder: 4

                onAnchorSelected: function (value) {
                    paramsModel.endAnchor = value
                }
                onOffsetEdited: function (value) {
                    paramsModel.endOffset = value
                }
            }
        }

        SeparatorLine {}

        BaseSection {
            id: tracksSection

            Layout.fillWidth: true

            title: qsTrc("trackedit", "Tracks")
            rowSpacing: prv.rowSpacing

            navigation.section: root.navigationSection
            navigation.order: 2

            ChoiceRow {
                title: qsTrc("trackedit", "Select")
                options: paramsModel.tracksKindOptions
                value: paramsModel.tracksKind
                navigationPanel: tracksSection.navigation
                navigationOrder: 1

                onChoiceSelected: function (value) {
                    paramsModel.tracksKind = value
                }
            }

            NumberRow {
                visible: paramsModel.tracksKind === "Range"

                title: qsTrc("trackedit", "First track")
                value: paramsModel.firstTrack
                navigationPanel: tracksSection.navigation
                navigationOrder: 2

                onNumberEdited: function (value) {
                    paramsModel.firstTrack = value
                }
            }

            NumberRow {
                visible: paramsModel.tracksKind === "Range"

                title: qsTrc("trackedit", "Track count")
                value: paramsModel.trackCount
                navigationPanel: tracksSection.navigation
                navigationOrder: 3

                onNumberEdited: function (value) {
                    paramsModel.trackCount = value
                }
            }

            ChoiceRow {
                visible: paramsModel.tracksKind === "Range"

                title: qsTrc("trackedit", "Mode")
                options: paramsModel.modeOptions
                value: paramsModel.mode
                navigationPanel: tracksSection.navigation
                navigationOrder: 4

                onChoiceSelected: function (value) {
                    paramsModel.mode = value
                }
            }
        }

        SeparatorLine {}

        RowLayout {
            Layout.fillWidth: true
            spacing: prv.itemSpacing

            RowLabel {
                text: qsTrc("trackedit", "Result")
            }

            StyledTextLabel {
                Layout.fillWidth: true

                text: paramsModel.summary
                opacity: 0.7
                horizontalAlignment: Text.AlignLeft
                wrapMode: Text.WordWrap
            }
        }

        SeparatorLine {
            Layout.leftMargin: -prv.margins
            Layout.rightMargin: -prv.margins
        }

        ButtonBox {
            Layout.fillWidth: true

            navigationPanel.section: root.navigationSection
            navigationPanel.order: prv.buttonsNavigationOrder

            FlatButton {
                Layout.preferredWidth: prv.buttonWidth
                Layout.preferredHeight: prv.buttonHeight

                text: qsTrc("global", "Cancel")
                buttonRole: ButtonBoxModel.RejectRole
                buttonId: ButtonBoxModel.Cancel

                onClicked: {
                    root.reject()
                }
            }

            FlatButton {
                Layout.preferredWidth: prv.buttonWidth
                Layout.preferredHeight: prv.buttonHeight

                text: qsTrc("global", "OK")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.Ok
                accentButton: true

                onClicked: {
                    prv.confirm()
                }
            }
        }
    }
}
