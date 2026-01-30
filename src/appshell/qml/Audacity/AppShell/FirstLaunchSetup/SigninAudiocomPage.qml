/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents
import Audacity.AppShell

Page {
    id: root

    title: qsTrc("appshell/gettingstarted", "Connect to your audio.com account")
    titleTopMargin: 16

    signal nextPage()

    QtObject {
        id: prv

        readonly property int columnSpacing: 16

        readonly property int socialButtonSpacing: 8
        readonly property int socialButtonHeight: 32
        readonly property int socialIconSize: 8
        readonly property int socialIconTextSpacing: 8

        readonly property string googleAuthProvider: "google"
        readonly property string facebookAuthProvider: "facebook"
        readonly property string googleTextLabel: qsTrc("appshell/gettingstarted", "Continue with Google")
        readonly property string facebookTextLabel: qsTrc("appshell/gettingstarted", "Continue with Facebook")
        readonly property string orUseEmailText: qsTrc("appshell/gettingstarted", "Or use email and password")

        readonly property int textSeparatorSpacing: 16
        readonly property int textInputTitleSpacing: 8
        readonly property int textInputHeight: 28
        readonly property string emailText: qsTrc("appshell/gettingstarted", "Email")
        readonly property string passwordText: qsTrc("appshell/gettingstarted", "Password")
        readonly property string forgotPasswordLink:  qsTrc("appshell/gettingstarted", "<a href=\"%1\">Forgot your password?</a>")
        readonly property string createNewAccountLink: qsTrc("appshell/gettingstarted", "Don´t have an account? <a href=\"create-account\">Create new account</a>")
        readonly property string alreadyHaveAccountLink: qsTrc("appshell/gettingstarted", "Already have an account? <a href=\"sign-in\">Sign in</a>")

        readonly property string formButtonTextLoading: qsTrc("appshell/gettingstarted", "Loading...")
        readonly property string formButtonTextSignIn: qsTrc("appshell/gettingstarted", "Sign In")
        readonly property string formButtonTextCreateAccount: qsTrc("appshell/gettingstarted", "Create Account")
        readonly property int formButtonHeight: 28
        readonly property int formButtonExtraSpace: model.showErrorMessage ? 0 : 8

        readonly property string forgotPasswordUrl: "https://audio.com/auth/forgot-password"
    }

    Component.onCompleted: {
        model.init()
    }

    SigninAudiocomPageModel{
        id: model

        onAuthorizedChanged: {
            if (authorized) {
                root.nextPage()
            }
        }
    }

    ColumnLayout {
        anchors.top: parent.top
        anchors.horizontalCenter: parent.horizontalCenter

        spacing: prv.columnSpacing

        RowLayout {
            id: socialButtonContainer

            spacing: prv.socialButtonSpacing

            NavigationPanel {
                id: socialButtonsPanel
                name: "SocialButtonsPanel"
                enabled: root.enabled && root.visible
                section: root.navigationSection
                order: root.navigationStartRow + 1
                direction: NavigationPanel.Horizontal
                accessible.name: qsTrc("appshell/gettingstarted", "Social sign-in options")
            }

            FlatButton {
                Layout.fillWidth: true
                Layout.preferredHeight: prv.socialButtonHeight

                navigation.name: "GoogleSignInButton"
                navigation.panel: socialButtonsPanel
                navigation.row: 0
                navigation.column: 0

                contentItem: RowLayout {
                    spacing: prv.socialIconTextSpacing

                    Image {
                        source: "qrc:/resources/GoogleLogo.png"
                        width: prv.socialIconSize
                        height: prv.socialIconSize
                        fillMode: Image.PreserveAspectFit
                    }

                    StyledTextLabel {
                        text: prv.googleTextLabel
                    }
                }

                onClicked: {
                    model.signInWithSocial(prv.googleAuthProvider)
                }
            }

            FlatButton {
                Layout.fillWidth: true
                Layout.preferredHeight: prv.socialButtonHeight

                navigation.name: "FacebookSignInButton"
                navigation.panel: socialButtonsPanel
                navigation.row: 0
                navigation.column: 1

                contentItem: RowLayout {
                    spacing: prv.socialIconTextSpacing

                    Image {
                        source: "qrc:/resources/FacebookLogo.png"
                        width: prv.socialIconSize
                        height: prv.socialIconSize
                        fillMode: Image.PreserveAspectFit
                    }

                    StyledTextLabel {
                        text: prv.facebookTextLabel
                    }
                }

                onClicked: {
                    model.signInWithSocial(prv.facebookAuthProvider)
                }
            }
        }

        RowLayout {
            spacing: prv.textSeparatorSpacing
            
            Layout.fillWidth: true
            Layout.preferredHeight: emailSeparatorText.height
            
            SeparatorLine {
                Layout.fillWidth: true
                orientation: Qt.Horizontal
            }

            StyledTextLabel {
                id: emailSeparatorText

                text: prv.orUseEmailText
                horizontalAlignment: Text.AlignHCenter
                font.pointSize: 12
            }

            SeparatorLine {
                Layout.fillWidth: true
                orientation: Qt.Horizontal
            }
        }

        Column {
            spacing: prv.textInputTitleSpacing
            Layout.fillWidth: true

            NavigationPanel {
                id: emailFieldPanel
                name: "EmailFieldsPanel"
                enabled: root.enabled && root.visible
                section: root.navigationSection
                direction: NavigationPanel.Vertical
                order: root.navigationStartRow + 2
                accessible.name: qsTrc("appshell/gettingstarted", "Email field")
            }

            StyledTextLabel {
                text: prv.emailText
            }

            TextInputField {
                id: emailInputField

                anchors.left: parent.left
                anchors.right: parent.right
                height: prv.textInputHeight

                navigation.name: "EmailInput"
                navigation.panel: emailFieldPanel
                navigation.row: 0
                navigation.column: 0
            }
        }

        Column {
            spacing: prv.textInputTitleSpacing
            Layout.fillWidth: true

             NavigationPanel {
                id: passwordFieldPanel
                name: "PasswordFieldsPanel"
                enabled: root.enabled && root.visible
                section: root.navigationSection
                direction: NavigationPanel.Vertical
                order: root.navigationStartRow + 3
                accessible.name: qsTrc("appshell/gettingstarted", "Password field")
            }

            RowLayout {
                width: parent.width

                StyledTextLabel {
                    text: prv.passwordText
                }

                Item {
                    Layout.fillWidth: true
                }

                StyledTextLabel {
                    visible: !model.isRegistering
                    text: prv.forgotPasswordLink.arg(prv.forgotPasswordUrl)
                }
            }

            TextInputField {
                id: passwordInputField

                anchors.left: parent.left
                anchors.right: parent.right
                height: prv.textInputHeight

                inputField.echoMode: TextInput.Password

                navigation.name: "PasswordInput"
                navigation.panel: passwordFieldPanel
                navigation.row: 0
                navigation.column: 0
            }
        }

        Column {
            spacing: 8
            Layout.fillWidth: true
            Layout.topMargin: prv.formButtonExtraSpace

            StyledTextLabel {
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.horizontalCenter: parent.horizontalCenter

                visible: model.showErrorMessage
                color: ui.theme.extra["record_color"]

                text: model.errorMessage
            }

            FlatButton {
                anchors.left: parent.left
                anchors.right: parent.right
                height: prv.formButtonHeight

                NavigationPanel {
                    id: actionsPanel
                    name: "ActionsPanel"
                    enabled: root.enabled && root.visible
                    section: root.navigationSection
                    order: root.navigationStartRow + 4
                    accessible.name: qsTrc("appshell/gettingstarted", "Form action")
                }

                accentButton: true

                text: {
                    if (model.isRegistering) {
                        return prv.formButtonTextCreateAccount
                    }

                    return model.authInProgress ? prv.formButtonTextLoading : prv.formButtonTextSignIn
                }

                navigation.name: "FormButton"
                navigation.panel: actionsPanel

                onClicked: {
                    model.triggerAction(emailInputField.inputField.text, passwordInputField.inputField.text)
                }
            }
        }

        StyledTextLabel {
            Layout.alignment: Qt.AlignHCenter
            text: model.isRegistering ? prv.alreadyHaveAccountLink : prv.createNewAccountLink
            textFormat: Text.RichText
            onLinkActivated: {
                if (link === "create-account") {
                    model.isRegistering = true
                    return
                }

                if (link === "sign-in") {
                    model.isRegistering = false
                    return
                }
            }
        }
    }
}
