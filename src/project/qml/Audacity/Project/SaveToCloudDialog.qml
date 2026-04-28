/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents
import Muse.Cloud 1.0

import Audacity.Cloud 1.0

StyledDialogView {
    id: root

    title: qsTrc("cloud/save", "Save to audio.com")

    contentWidth: prv.contentWidth
    contentHeight: content.implicitHeight

    property string formTitle: qsTrc("cloud", "Project name")
    property string actionText: qsTrc("cloud", "Save")

    QtObject {
        id: prv

        readonly property int contentWidth: 367

        readonly property string signoutButtonText: qsTrc("cloud", "Sign out")
        readonly property int signoutButtonHeight: 28
        readonly property int signoutButtonWidth: 71

        readonly property int accountRowMargins: 8
        readonly property int accountRowSpacing: 8
        readonly property int accountAvatarSide: 36
        readonly property string notSignedInText: qsTrc("cloud", "You are not signed in")

        readonly property int formColumnMargins: 16
        readonly property int formColumnSpacing: 8
        readonly property int formTextInputHeight: 28

        readonly property int buttonBoxMargins: 8

        readonly property bool canTrigger: newProjectModel.isFilenameAllowed(projectNameField.value)

        function onTriggered() {
            if (model.isAuthorized) {
                root.ret = {
                    errcode: 0,
                    value: projectNameField.value
                }
                root.hide()
            } else {
                model.openSignInDialog()
            }
        }
    }

    AccountModel {
        id: model
    }

    NewProjectModel {
        id: newProjectModel
    }

    Component.onCompleted: {
        model.init()
        Qt.callLater(projectNameField.ensureActiveFocus)
    }

    ColumnLayout {
        id: content

        anchors.fill: parent
        spacing: 0

        NavigationPanel {
            id: accountNavPanel
            name: "AccountPanel"
            enabled: accountInfoRow.enabled && accountInfoRow.visible
            section: root.navigationSection
            order: 1
        }

        RowLayout {
            id: accountInfoRow

            Layout.fillWidth: true
            Layout.margins: prv.accountRowMargins

            spacing: prv.accountRowSpacing

            AccountAvatar {
                id: avatar
                side: 36
                url: model.avatarPath
            }

            StyledTextLabel {
                Layout.fillWidth: true

                font: ui.theme.bodyFont
                text: model.isAuthorized ? model.displayName : prv.notSignedInText

                horizontalAlignment: Text.AlignLeft
            }

            Item {
                Layout.fillWidth: true
            }

            FlatButton {
                id: signoutButton

                Layout.preferredHeight: prv.signoutButtonHeight
                Layout.preferredWidth: prv.signoutButtonWidth

                text: prv.signoutButtonText

                visible: model.isAuthorized

                navigation.panel: accountNavPanel
                navigation.order: 1

                onClicked: function () {
                    model.signOut()
                }
            }
        }

        SeparatorLine {
            Layout.fillWidth: true
        }

        ColumnLayout {
            id: projectNameColumn

            Layout.fillWidth: true
            Layout.margins: prv.formColumnMargins

            spacing: prv.formColumnSpacing

            StyledTextLabel {
                Layout.fillWidth: true

                font: ui.theme.bodyFont
                text: root.formTitle

                horizontalAlignment: Text.AlignLeft
            }

            NavigationPanel {
                id: projectNameNavPanel

                name: "ProjectNamePanel"
                enabled: projectNameColumn.enabled && projectNameColumn.visible
                section: root.navigationSection
                order: 2
            }

            TextInputField {
                id: projectNameField

                property string value: ""

                Layout.fillWidth: true

                Layout.preferredHeight: prv.formTextInputHeight

                navigation.panel: projectNameNavPanel
                navigation.order: 1

                onTextChanged: function (newTextValue) {
                    value = newTextValue
                }

                onAccepted: function () {
                    if (prv.canTrigger) {
                        prv.onTriggered()
                    } else {
                        Qt.callLater(projectNameField.ensureActiveFocus)
                    }
                }
            }
        }

        SeparatorLine {
            Layout.fillWidth: true
        }

        ButtonBox {
            id: buttonBox

            Layout.fillWidth: true
            Layout.margins: prv.buttonBoxMargins

            buttons: [ButtonBoxModel.Cancel]

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 3

            FlatButton {
                id: saveButton

                text: root.actionText
                buttonRole: ButtonBoxModel.ApplyRole
                buttonId: ButtonBoxModel.Apply
                enabled: prv.canTrigger

                onClicked: function () {
                    prv.onTriggered()
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
