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

    QtObject {
        id: prv

        readonly property int columnSpacing: 16

        readonly property int socialButtonSpacing: 8
        readonly property int socialButtonHeight: 32
        readonly property int socialIconTextSpacing: 8

        readonly property string googleAuthProvider: "google"
        readonly property string facebookAuthProvider: "facebook"
        readonly property string googleTextLabel: qsTrc("appshell/gettingstarted", "Continue with Google")
        readonly property string facebookTextLabel: qsTrc("appshell/gettingstarted", "Continue with Facebook")
        readonly property string orUseEmailText: qsTrc("appshell/gettingstarted", "Or use email and password")
        readonly property int providerLogoSize: 16

        readonly property int textSeparatorSpacing: 16
        readonly property int textInputTitleSpacing: 8
        readonly property int textInputHeight: 28
        readonly property string emailText: qsTrc("appshell/gettingstarted", "Email")
        readonly property string passwordText: qsTrc("appshell/gettingstarted", "Password")
        readonly property string forgotPasswordLink:  qsTrc("appshell/gettingstarted", "<a href=\"%1\">Forgot your password?</a>")
        
        readonly property string noAccountText: qsTrc("appshell/gettingstarted", "Don't have an account?")
        readonly property string createAccountLinkText: qsTrc("appshell/gettingstarted", "Create new account")
        readonly property string haveAccountText: qsTrc("appshell/gettingstarted", "Already have an account?")
        readonly property string signInLinkText: qsTrc("appshell/gettingstarted", "Sign in")
        readonly property int textLinkSpacing: 4

        readonly property string formButtonTextLoading: qsTrc("appshell/gettingstarted", "Loading...")
        readonly property string formButtonTextSignIn: qsTrc("appshell/gettingstarted", "Sign in")
        readonly property string formButtonTextCreateAccount: qsTrc("appshell/gettingstarted", "Create account")
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
                root.navNextPageRequested()
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
                        fillMode: Image.PreserveAspectFit

                        Layout.preferredWidth: prv.providerLogoSize
                        Layout.preferredHeight: prv.providerLogoSize
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
                        fillMode: Image.PreserveAspectFit

                        Layout.preferredWidth: prv.providerLogoSize
                        Layout.preferredHeight: prv.providerLogoSize
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

            RowLayout {
                width: parent.width

                StyledTextLabel {
                    text: prv.passwordText
                }

                Item {
                    Layout.fillWidth: true
                }

                FocusableControl {
                    visible: !model.isRegistering
                    
                    implicitWidth: forgotPasswordLabel.implicitWidth
                    implicitHeight: forgotPasswordLabel.implicitHeight
                    
                    background.color: "transparent"
                    background.border.width: 0
                    
                    NavigationPanel {
                        id: forgetPasswordPanel
                        name: "ForgotPasswordPanel"
                        enabled: root.enabled && root.visible
                        section: root.navigationSection
                        direction: NavigationPanel.Vertical
                        order: root.navigationStartRow + 3
                        accessible.name: qsTrc("appshell/gettingstarted", "Forgot password")
                    }

                    navigation.name: "ForgotPasswordLink"
                    navigation.panel: forgetPasswordPanel
                    navigation.row: 0
                    navigation.column: 0
                    navigation.accessible.name: qsTrc("appshell/gettingstarted", "Forgot password")

                    onNavigationTriggered: {
                        Qt.openUrlExternally(prv.forgotPasswordUrl)
                    }
                    
                    StyledTextLabel {
                        id: forgotPasswordLabel
                        anchors.fill: parent
                        text: prv.forgotPasswordLink.arg(prv.forgotPasswordUrl)
                        textFormat: Text.RichText
                    }
                }
            }

            TextInputField {
                id: passwordInputField

                anchors.left: parent.left
                anchors.right: parent.right
                height: prv.textInputHeight

                inputField.echoMode: TextInput.Password

                NavigationPanel {
                    id: passwordFieldPanel
                    name: "PasswordFieldsPanel"
                    enabled: root.enabled && root.visible
                    section: root.navigationSection
                    direction: NavigationPanel.Vertical
                    order: root.navigationStartRow + 4
                    accessible.name: qsTrc("appshell/gettingstarted", "Password field")
                }

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

                visible: model.showErrorMessage
                color: ui.theme.extra["error_text_color"]

                text: model.errorMessage
            }

            FlatButton {
                anchors.left: parent.left
                anchors.right: parent.right
                height: prv.formButtonHeight

                enabled: !model.authInProgress 
                          && emailInputField.inputField.text.length > 0
                          && passwordInputField.inputField.text.length > 0

                NavigationPanel {
                    id: actionsPanel
                    name: "ActionsPanel"
                    enabled: root.enabled && root.visible
                    section: root.navigationSection
                    order: root.navigationStartRow + 5
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
                    model.isRegistering ?
                        model.signUpWithEmail(emailInputField.inputField.text, passwordInputField.inputField.text) :
                        model.signInWithEmail(emailInputField.inputField.text, passwordInputField.inputField.text)
                }
            }
        }

        RowLayout {
            Layout.alignment: Qt.AlignHCenter
            spacing: prv.textLinkSpacing
            
            StyledTextLabel {
                text: model.isRegistering ? prv.haveAccountText : prv.noAccountText
            }
            
            FocusableControl {
                implicitWidth: accountLinkLabel.implicitWidth
                implicitHeight: accountLinkLabel.implicitHeight
                
                background.color: "transparent"
                background.border.width: 0
                
                NavigationPanel {
                    id: accountLinkPanel
                    name: "AccountLinkPanel"
                    enabled: root.enabled && root.visible
                    section: root.navigationSection
                    direction: NavigationPanel.Vertical
                    order: root.navigationStartRow + 6
                    accessible.name: model.isRegistering 
                        ? qsTrc("appshell/gettingstarted", "Sign in link") 
                        : qsTrc("appshell/gettingstarted", "Create account link")
                }
                
                navigation.name: "AccountLink"
                navigation.panel: accountLinkPanel
                navigation.row: 0
                navigation.column: 0
                navigation.accessible.name: model.isRegistering ? prv.signInLinkText : prv.createAccountLinkText
                
                onNavigationTriggered: {
                    model.isRegistering = !model.isRegistering
                }
                
                StyledTextLabel {
                    id: accountLinkLabel
                    anchors.fill: parent
                    text: model.isRegistering ? prv.signInLinkText : prv.createAccountLinkText
                    color: ui.theme.linkColor
                    font.underline: true

                    MouseArea {
                        anchors.fill: parent
                        cursorShape: Qt.PointingHandCursor

                        onClicked: {
                            model.isRegistering = !model.isRegistering
                        }
                    }
                }
            }
        }
    }
}
