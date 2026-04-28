/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.Cloud 1.0

FocusScope {
    id: root

    QtObject {
        id: prv

        readonly property string audioComText: qsTrc("cloud", "Audio.com")

        readonly property int titleItemSpacing: 40
        readonly property int sideMargin: 46

        readonly property real actualCellWidth: 500
        readonly property real actualCellHeight: 200
    }

    AccountModel {
        id: model
    }

    Component.onCompleted: {
        model.init()
    }

    NavigationSection {
        id: navSec
        name: "Account"
        enabled: root.enabled && root.visible
        order: 3
        onActiveChanged: function (active) {
            if (active) {
                root.forceActiveFocus()
            }
        }
    }

    Rectangle {
        id: background
        anchors.fill: parent
        color: ui.theme.backgroundSecondaryColor
    }

    StyledTextLabel {
        id: pageTitle

        anchors.top: parent.top
        anchors.topMargin: prv.sideMargin
        anchors.left: parent.left
        anchors.leftMargin: prv.sideMargin
        anchors.right: parent.right
        anchors.rightMargin: prv.sideMargin

        text: qsTrc("appshell", "Cloud account")
        font: ui.theme.titleBoldFont
        horizontalAlignment: Text.AlignLeft
    }

    CloudItem {
        width: prv.actualCellWidth
        height: prv.actualCellHeight

        anchors.top: pageTitle.bottom
        anchors.topMargin: prv.titleItemSpacing
        anchors.left: parent.left
        anchors.leftMargin: prv.sideMargin

        cloudTitle: prv.audioComText
        userIsAuthorized: model.isAuthorized
        userName: model.displayName
        userAvatarUrl: model.avatarPath

        onSignInRequested: {
            model.openSignInDialog()
        }

        onSignOutRequested: {
            model.signOut()
        }

        onCreateAccountRequested: {
            model.openCreateAccountDialog()
        }
    }
}
