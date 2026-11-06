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
    property var existingNames: []

    title: qsTrc("macromanager", "New macro")

    contentWidth: prv.contentWidth
    margins: prv.margins

    QtObject {
        id: prv

        readonly property int contentWidth: 320
        readonly property int margins: 12
        readonly property int fieldTopMargin: 4
        readonly property int errorTopMargin: 6
        readonly property int buttonsTopMargin: 16
        readonly property int buttonWidth: 64
        readonly property int buttonHeight: 28

        property string initialName: ""

        readonly property string trimmedName: root.macroName.trim()
        readonly property bool nameExists: prv.trimmedName !== prv.initialName && root.existingNames.includes(prv.trimmedName)
        readonly property bool isNameValid: prv.trimmedName.length > 0 && !prv.nameExists

        readonly property color errorColor: ui.theme.extra["error_text_color"]

        function confirm() {
            root.ret = {
                errcode: 0,
                value: prv.trimmedName
            }
            root.hide()
        }
    }

    Component.onCompleted: {
        prv.initialName = root.macroName.trim()
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

            Rectangle {
                anchors.fill: parent

                visible: prv.nameExists

                color: "transparent"
                radius: nameField.background.radius
                border.color: prv.errorColor
                border.width: nameField.background.border.width
            }
        }

        StyledTextLabel {
            Layout.topMargin: prv.errorTopMargin
            Layout.fillWidth: true

            visible: prv.nameExists

            text: qsTrc("macromanager", "A macro with this name already exists")
            color: prv.errorColor
            horizontalAlignment: Qt.AlignLeft
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
