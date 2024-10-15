/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Muse.Diagnostics 1.0

Rectangle {
    color: ui.theme.backgroundSecondaryColor

    CrashHandlerDevToolsModel {
        id: model
    }

    Column {
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.margins: 12

        spacing: 12

        Rectangle {
            readonly property real padding: 12

            width: hintLabel.implicitWidth + 2 * padding
            height: hintLabel.implicitHeight + 2 * padding

            color: Utils.colorWithAlpha(border.color, 0.25)
            border.color: "red"
            border.width: 1
            radius: 6

            Row {
                id: hintLabel
                anchors.fill: parent
                anchors.margins: parent.padding
                spacing: 4

                StyledIconLabel {
                    iconCode: IconCode.WARNING
                }

                StyledTextLabel {
                    // Just for fun... or when a user accidentally sees this (shouldn't happen)
                    text: "DANGER! This button does exactly what it says!"
                }
            }
        }

        FlatButton {
            text: "Crash"
            onClicked: model.doCrash()
        }
    }
}
