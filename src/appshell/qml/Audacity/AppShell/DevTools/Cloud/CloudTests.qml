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

    CloudTestsModel {
        id: model
    }

    Component.onCompleted: {
        model.init();
    }

    Flickable {
        id: flickable

        anchors.fill: parent
        anchors.margins: 12

        contentHeight: contentColumn.implicitHeight

        Column {
            id: contentColumn

            width: parent.width 
            height: implicitHeight
            spacing: 36

            StyledTextLabel {
                text: "Cloud Tests"
                font: ui.theme.largeBodyFont
            }

            ColumnLayout {
                spacing: 8

                Row {
                    spacing: 8

                    Rectangle {
                        visible: true
                        width: 32
                        height: 32
                        color: model.isAuthorized ? "green" : "red"
                    }

                    StyledTextLabel {
                        text: model.isAuthorized ? model.displayName : "User not logged in"
                        font: ui.theme.bodyFont
                    }
                }

                FlatButton {
                    text: "Logout"

                    onClicked: {
                        model.signOut();
                    }
                }
            }
        }
    }

}