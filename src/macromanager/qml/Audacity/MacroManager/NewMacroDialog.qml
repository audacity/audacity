/*
* Audacity: A Digital Audio Editor
*/
pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

StyledDialogView {
    id: root

    title: qsTrc("macromanager", "New macro")

    contentWidth: prv.contentWidth
    margins: prv.margins

    QtObject {
        id: prv

        readonly property int contentWidth: 320
        readonly property int margins: 12
        readonly property int fieldTopMargin: 4
        readonly property int buttonsTopMargin: 16

        property string macroName: ""
        readonly property bool isNameValid: macroName.trim().length > 0
    }

    onNavigationActivateRequested: {
        nameField.navigation.requestActive()
    }

    ColumnLayout {
        id: content

        width: parent.width

        spacing: 0

        property NavigationPanel navigationPanel: NavigationPanel {
            name: "NewMacroPanel"
            direction: NavigationPanel.Vertical
            section: root.navigationSection
            order: 1
        }

        StyledTextLabel {
            id: nameTitle

            text: qsTrc("macromanager", "Macro name")
        }

        TextInputField {
            id: nameField

            Layout.topMargin: prv.fieldTopMargin
            Layout.fillWidth: true

            navigation.name: "MacroNameInputField"
            navigation.panel: content.navigationPanel
            navigation.row: 1
            navigation.accessible.name: nameTitle.text + " " + currentText

            currentText: prv.macroName

            onTextChanged: function (newTextValue) {
                prv.macroName = newTextValue
            }

            onAccepted: {
                if (prv.isNameValid) {
                    root.ret = {
                        errcode: 0,
                        value: prv.macroName.trim()
                    }
                    root.hide()
                }
            }
        }

        ButtonBox {
            Layout.fillWidth: true
            Layout.topMargin: prv.buttonsTopMargin

            buttons: [ButtonBoxModel.Cancel]

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 2

            FlatButton {
                text: qsTrc("macromanager", "Confirm")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.CustomButton + 1
                accentButton: true
                enabled: prv.isNameValid

                onClicked: {
                    root.ret = {
                        errcode: 0,
                        value: prv.macroName.trim()
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
