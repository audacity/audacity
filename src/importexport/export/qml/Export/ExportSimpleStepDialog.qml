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

    title: simpleStepModel.title

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
                value: simpleStepModel.stepData()
            }
            root.hide()
        }
    }

    ExportSimpleStepModel {
        id: simpleStepModel
    }

    Component.onCompleted: {
        simpleStepModel.load(root.stepData)
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

    ColumnLayout {
        id: content

        width: parent.width
        spacing: prv.sectionsSpacing

        ExportStepFileSection {
            id: fileSection

            Layout.fillWidth: true

            stepModel: simpleStepModel
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

            RowLayout {
                width: parent.width
                spacing: prv.itemSpacing

                RowLabel {
                    id: presetLabel
                    text: qsTrc("export", "Use")
                }

                StyledDropdown {
                    id: presetDropdown

                    Layout.preferredWidth: prv.dropdownWidth

                    model: simpleStepModel.presetOptions
                    currentIndex: presetDropdown.indexOfValue(simpleStepModel.preset)

                    navigation.panel: formatSection.navigation
                    navigation.order: 1
                    navigation.accessible.name: presetLabel.text + ": " + presetDropdown.currentText

                    onActivated: function (index, value) {
                        simpleStepModel.setPreset(value)
                    }
                }
            }

            RowLayout {
                width: parent.width
                spacing: prv.itemSpacing

                RowLabel {}

                StyledTextLabel {
                    Layout.fillWidth: true

                    text: simpleStepModel.presetResult
                    opacity: 0.7
                    horizontalAlignment: Text.AlignLeft
                    wrapMode: Text.WordWrap
                }
            }
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
