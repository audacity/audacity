/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15
import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.Toast 1.0
import Audacity.Cloud 1.0

Rectangle {
    id: root

    color: ui.theme.backgroundPrimaryColor

    QtObject {
        id: prv

        readonly property int mainMargin: 12
        readonly property int groupBoxMargin: 24
        readonly property int iconSize: 48
        readonly property int iconTextSpacing: 12
        readonly property int buttonWidth: 100
        readonly property int infoButtonSpacing: 24
    }

    CloudTestsModel {
        id: model
    }

    Component.onCompleted: {
        model.init();
    }

    Flickable {
        id: flickable

        anchors.top: parent.top
        anchors.left: parent.left
        anchors.margins: prv.mainMargin

        contentHeight: groupBox.implicitHeight

        StyledGroupBox {
            id: groupBox

            anchors.margins: prv.groupBoxMargin

            title: "Account Information"
            height: implicitHeight
            width: implicitWidth

            Column {
                id: contentColumn

                spacing: prv.infoButtonSpacing

                Row {
                    visible: model.isAuthorized
                    spacing: prv.iconTextSpacing

                    Item {
                        id: userAvatarContainer

                        width: prv.iconSize
                        height: prv.iconSize

                        StyledIconLabel {
                            anchors.fill: parent

                            visible: model.avatarPath === ""
                            iconCode: IconCode.ACCOUNT
                        }

                        Image {
                            anchors.fill: parent
                            visible: model.avatarPath !== ""
                            source: model.avatarPath
                        }
                    }

                    StyledTextLabel {
                        anchors.verticalCenter: parent.verticalCenter

                        font: ui.theme.bodyFont
                        text: model.displayName
                    }
                }

                Row {
                    visible: !model.isAuthorized
                    spacing: prv.iconTextSpacing

                    Rectangle {
                        width: prv.iconSize
                        height: prv.iconSize

                        visible: !model.isAuthorized
                        color: "red"
                    }

                    StyledTextLabel {
                        anchors.verticalCenter: parent.verticalCenter

                        text: "User not authorized"
                        font: ui.theme.bodyFont
                    }
                }

                FlatButton {
                    text: "Logout"

                    anchors.horizontalCenter: parent.horizontalCenter

                    width: prv.buttonWidth

                    onClicked: {
                        model.signOut();
                    }
                }
            }
        }
    }
}