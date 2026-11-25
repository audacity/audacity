/*
* Audacity: A Digital Audio Editor
*/
pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.UiComponents 1.0
import Audacity.Export 1.0

import "internal"

StyledDialogView {
    id: root

    property var stepData: ({})

    title: audioStepModel.title

    contentWidth: prv.contentWidth
    contentHeight: content.implicitHeight
    margins: prv.margins

    QtObject {
        id: prv

        readonly property int contentWidth: 540
        readonly property int margins: 12
        readonly property int labelColumnWidth: 140
        readonly property int dropdownWidth: 260
        readonly property int sectionsSpacing: 16
        readonly property int rowSpacing: 8
        readonly property int itemSpacing: 8
        readonly property int buttonWidth: 64
        readonly property int buttonHeight: 28
        readonly property int buttonsNavigationOrder: 1000

        function confirm() {
            root.ret = {
                errcode: 0,
                value: audioStepModel.stepData()
            }
            root.hide()
        }
    }

    ExportAudioStepModel {
        id: audioStepModel
    }

    Component.onCompleted: {
        audioStepModel.load(root.stepData)
    }

    onNavigationActivateRequested: {
        fileSection.requestFileNameActive()
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

        required property int index
        required property string title
        required property int section
        required property var options
        required property string value

        property int shownSection: ExportAudioStepModel.Format
        property NavigationPanel navigationPanel: null

        visible: choiceRow.section === choiceRow.shownSection
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
            navigation.order: choiceRow.index
            navigation.accessible.name: choiceRow.title + ": " + dropdown.currentText

            onActivated: function (index, value) {
                Qt.callLater(audioStepModel.setValue, choiceRow.index, value)
            }
        }
    }

    ColumnLayout {
        id: content

        width: parent.width
        spacing: prv.sectionsSpacing

        ExportStepFileSection {
            id: fileSection

            Layout.fillWidth: true

            stepModel: audioStepModel
            labelColumnWidth: prv.labelColumnWidth
            itemSpacing: prv.itemSpacing
            rowSpacing: prv.rowSpacing

            navigation.section: root.navigationSection
            navigation.order: 1
        }

        SeparatorLine {}

        BaseSection {
            id: formatSection

            Layout.fillWidth: true

            //: Section heading in the dialog of the export macro step
            title: qsTrc("export", "Format")
            rowSpacing: prv.rowSpacing

            navigation.section: root.navigationSection
            navigation.order: 2

            Repeater {
                model: audioStepModel

                delegate: ChoiceRow {
                    shownSection: ExportAudioStepModel.Format
                    navigationPanel: formatSection.navigation
                }
            }
        }

        SeparatorLine {}

        BaseSection {
            id: audioSection

            Layout.fillWidth: true

            //: Section heading in the dialog of the export macro step
            title: qsTrc("export", "Audio options")
            rowSpacing: prv.rowSpacing

            navigation.section: root.navigationSection
            navigation.order: 3

            Repeater {
                model: audioStepModel

                delegate: ChoiceRow {
                    shownSection: ExportAudioStepModel.Audio
                    navigationPanel: audioSection.navigation
                }
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
