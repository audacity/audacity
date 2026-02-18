/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.Ui
import Muse.UiComponents
import Muse.Cloud

Item {
    id: root

    property string cloudTitle: ""
    property bool userIsAuthorized: false
    property string userName: ""
    property url userProfileUrl
    property url userAvatarUrl
    property url userCollectionUrl

    property NavigationPanel navigationPanel: NavigationPanel {
        name: root.cloudTitle + "Item"
        direction: NavigationPanel.Both

        onActiveChanged: function(active) {
            if (active) {
                firstButton.navigation.requestActive()
                accessibleInfo.ignored = false
                accessibleInfo.focused = true
            } else {
                accessibleInfo.ignored = true
                accessibleInfo.focused = false
                firstButton.accessible.ignored = true
            }
        }
    }

    signal signInRequested()
    signal signOutRequested()
    signal createAccountRequested()

    AccessibleItem {
        id: accessibleInfo
        accessibleParent: root.navigationPanel.accessible
        visualItem: root
        role: MUAccessible.Button
        name: {
            var msg = ""
            if (Boolean(root.userIsAuthorized)) {
                msg = "%1. %2. %3. %4".arg(root.cloudTitle)
                .arg(root.userName)
                .arg(root.userCollectionUrl)
                .arg(firstButton.text)
            } else {
                msg = "%1. %2. %3".arg(root.cloudTitle)
                .arg(qsTrc("cloud", "Not signed in"))
                .arg(firstButton.text)
            }

            return msg
        }
    }

    StyledTextLabel {
        id: cloudTitleLabel

        anchors.left: parent.left
        anchors.right: parent.right

        text: root.cloudTitle

        font: ui.theme.tabBoldFont
        horizontalAlignment: Text.AlignLeft
    }

    Rectangle {
        anchors.top: cloudTitleLabel.bottom
        anchors.topMargin: 20
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom

        color: ui.theme.backgroundPrimaryColor

        radius: 12
        border.width: 1
        border.color: ui.theme.strokeColor

        Item {
            anchors.fill: parent
            anchors.margins: 24

            AccountAvatar {
                id: avatar

                url: root.userAvatarUrl
                side: 100
            }

            Column {
                anchors.top: parent.top
                anchors.left: avatar.right
                anchors.leftMargin: 24
                anchors.right: parent.right

                spacing: 12

                StyledTextLabel {
                    anchors.left: parent.left
                    anchors.right: parent.right

                    text: Boolean(root.userIsAuthorized) ? root.userName : qsTrc("cloud", "Not signed in")

                    font: ui.theme.headerBoldFont
                    horizontalAlignment: Text.AlignLeft
                }

                StyledTextLabel {
                    anchors.left: parent.left
                    anchors.right: parent.right

                    text: Boolean(root.userIsAuthorized) ? root.userCollectionUrl : root.cloudTitle

                    font: ui.theme.tabFont
                    horizontalAlignment: Text.AlignLeft
                }
            }

            Row {
                spacing: 12

                anchors.left: avatar.right
                anchors.leftMargin: 24
                anchors.right: parent.right
                anchors.bottom: parent.bottom

                FlatButton {
                    id: firstButton

                    width: (parent.width - parent.spacing) / 2

                    text: Boolean(root.userIsAuthorized) ? qsTrc("cloud", "My profile") : qsTrc("cloud", "Sign in")
                    accentButton: true

                    navigation.panel: root.navigationPanel
                    navigation.name: "FirstButton"
                    navigation.order: 1
                    navigation.accessible.ignored: true
                    navigation.onActiveChanged: {
                        if (!navigation.active) {
                            accessible.ignored = false
                            accessibleInfo.ignored = true
                        }
                    }

                    onClicked: {
                        if (Boolean(root.userIsAuthorized)) {
                            api.launcher.openUrl(root.userProfileUrl)
                        } else {
                            root.signInRequested()
                        }
                    }
                }

                FlatButton {
                    id: secondButton

                    width: (parent.width - parent.spacing) / 2

                    text: Boolean(root.userIsAuthorized) ? qsTrc("cloud", "Sign out") : qsTrc("cloud", "Create account")
                    accentButton: !Boolean(root.userIsAuthorized)

                    navigation.panel: root.navigationPanel
                    navigation.name: "SecondButton"
                    navigation.order: 2

                    onClicked: {
                        if (Boolean(root.userIsAuthorized)) {
                            root.signOutRequested()
                        } else {
                            root.createAccountRequested()
                        }
                    }
                }
            }
        }
    }
}