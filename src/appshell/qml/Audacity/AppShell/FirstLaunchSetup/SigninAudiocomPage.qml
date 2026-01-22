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
    titleTopMargin: 24

    QtObject {
        id: prv

        readonly property int columnSpacing: 16
        
        readonly property int socialButtonSpacing: 8
        readonly property int socialButtonHeight: 32
        readonly property int socialIconSize: 8
        readonly property int socialIconTextSpacing: 8
        readonly property string googleTextLabel: qsTrc("appshell/gettingstarted", "Continue with Google")
        readonly property string facebookTextLabel: qsTrc("appshell/gettingstarted", "Continue with Facebook")
        readonly property string orUseEmailText: qsTrc("appshell/gettingstarted", "Or use email and password")

        readonly property int textSeparatorSpacing: 16
        readonly property int textInputTitleSpacing: 8
        readonly property int textInputHeight: 28
        readonly property string emailText: qsTrc("appshell/gettingstarted", "Email")
        readonly property string passwordText: qsTrc("appshell/gettingstarted", "Password")
        readonly property string noAccountText: qsTrc("appshell/gettingstarted", "Don't have an account? Create a new account.")
        readonly property string signInText: qsTrc("appshell/gettingstarted", "Sign In")
        readonly property int signinButtonHeight: 28

        readonly property string forgotPasswordUrl: "https://audio.com/auth/forgot-password"
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

        ColumnLayout {
            Layout.fillWidth: true
            spacing: prv.columnSpacing

            NavigationPanel {
                id: formFieldsPanel
                name: "FormFieldsPanel"
                enabled: root.enabled && root.visible
                section: root.navigationSection
                direction: NavigationPanel.Vertical
                order: root.navigationStartRow + 2
                accessible.name: qsTrc("appshell/gettingstarted", "Email and password field")
            }

            Column {
                spacing: prv.textInputTitleSpacing
                Layout.fillWidth: true

                StyledTextLabel {
                    text: prv.emailText
                }

                TextInputField {
                    anchors.left: parent.left
                    anchors.right: parent.right
                    height: prv.textInputHeight

                    navigation.name: "EmailInput"
                    navigation.panel: formFieldsPanel
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

                    StyledTextLabel {
                        text: qsTrc("appshell/gettingstarted", "<a href=\"%1\">Forgot your password?</a>")
                            .arg(prv.forgotPasswordUrl)
                    }
                }

                TextInputField {
                    anchors.left: parent.left
                    anchors.right: parent.right
                    height: prv.textInputHeight

                    inputField.echoMode: TextInput.Password

                    navigation.name: "PasswordInput"
                    navigation.panel: formFieldsPanel
                    navigation.row: 1
                    navigation.column: 0
                }
            }
        }

        FlatButton {
            Layout.topMargin: 8
            Layout.fillWidth: true
            Layout.preferredHeight: prv.signinButtonHeight

            NavigationPanel {
                id: actionsPanel
                name: "ActionsPanel"
                enabled: root.enabled && root.visible
                section: root.navigationSection
                order: root.navigationStartRow + 3
                accessible.name: qsTrc("appshell/gettingstarted", "Sign-in action")
            }

            accentButton: true

            text: prv.signInText

            navigation.name: "SignInButton"
            navigation.panel: actionsPanel
        }

        StyledTextLabel {
            Layout.alignment: Qt.AlignHCenter

            text: prv.noAccountText
        }
    }
}