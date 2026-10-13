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

    property string macroName: ""

    title: qsTrc("macromanager", "New macro")

    contentWidth: prv.contentWidth
    margins: prv.margins

    QtObject {
        id: prv

        readonly property int contentWidth: 320
        readonly property int margins: 12
        readonly property int fieldTopMargin: 4
        readonly property int buttonsTopMargin: 16
        readonly property int buttonWidth: 64
        readonly property int buttonHeight: 28

        readonly property bool isNameValid: root.macroName.trim().length > 0

        function confirm() {
            root.ret = {
                errcode: 0,
                value: root.macroName.trim()
            }
            root.hide()
        }
    }

    onNavigationActivateRequested: {
        nameField.navigation.requestActive()
    }

    ColumnLayout {
        id: content

        width: parent.width

        spacing: 0

        property NavigationPanel navigationPanel: NavigationPanel {
            name: "MacroNamePanel"
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

            currentText: root.macroName

            onTextChanged: function (newTextValue) {
                root.macroName = newTextValue
            }

            onAccepted: {
                if (prv.isNameValid) {
                    prv.confirm()
                }
            }

            Component.onCompleted: {
                selectAll()
            }
        }

        ButtonBox {
            Layout.fillWidth: true
            Layout.topMargin: prv.buttonsTopMargin

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 2

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

                text: qsTrc("macromanager", "Confirm")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.CustomButton + 1
                accentButton: true
                enabled: prv.isNameValid

                onClicked: {
                    prv.confirm()
                }
            }
        }
    }
}
