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

    title: prv.dialogTitle

    contentWidth: prv.contentWidth
    contentHeight: content.implicitHeight

    QtObject {
        id: prv

        readonly property int contentWidth: 367

        readonly property string dialogTitle: qsTrc("project/save", "Save to audio.com")

        readonly property string signoutButtonText: qsTrc("project", "Sign out")
        readonly property int signoutButtonHeight: 28
        readonly property int signoutButtonWidth: 71

        readonly property int accountRowMargins: 8
        readonly property int accountRowSpacing: 8
        readonly property int accountAvatarSide: 36
        readonly property string notSignedInText: qsTrc("project", "You are not signed in")

        readonly property int formColumnMargins: 16
        readonly property int formColumnSpacing: 8
        readonly property int formTextInputHeight: 28
        readonly property string formTitle: qsTrc("project", "Project name")

        readonly property string saveButtonText: qsTrc("global", "Save")

        readonly property int buttonBoxMargins: 8
    }

    AccountInfoModel {
        id: model
    }

    Component.onCompleted: {
        model.init()
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

                onClicked: function() {
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
                text: prv.formTitle

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

                onTextChanged: function(newTextValue) {
                    value = newTextValue
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

            buttons: [ ButtonBoxModel.Cancel ]

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 3

            FlatButton {
                id: saveButton

                text: prv.saveButtonText
                buttonRole: ButtonBoxModel.ApplyRole
                buttonId: ButtonBoxModel.Apply
                enabled: projectNameField.hasText

                onClicked: function() {
                    if (model.isAuthorized) {
                        root.ret = { errcode: 0, value: projectNameField.value }
                        root.hide()
                    } else {
                        model.openAuthorizationDialog()
                    }
                }
            }

            onStandardButtonClicked: function(buttonId) {
                if (buttonId === ButtonBoxModel.Cancel) {
                    root.reject()
                }
            }
        }
    }
}
