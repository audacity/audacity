/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import "."

Item {
    id: root

    property int iconCode: IconCode.NONE
    property string title: ""
    property string message: ""
    property bool dismissable: true
    property var actions: []

    property int progress: 0
    property bool showProgressInfo: false
    property int timeElapsed: 0

    property int verticalMargin: 12
    property int rightMargin: root.dismissable ? 12 : 24
    property int leftMargin: 12
    property int titleMessageSpacing: 4
    property int iconContentSpacing: 12
    property int textContentSpacing: 12
    property int dismissButtonSize: 14
    property int iconSize: 12
    property int titlePixelSize: 14
    property int messagePixelSize: 14
    property int actionButtonHeight: 28
    property int actionButtonMargins: 6
    property int actionButtonsSpacing: 8

    signal dismissed()
    signal actionTriggered(string actionStr)

    width: 360
    height: implicitHeight

    implicitHeight: mainContainer.childrenRect.height + 24

    Rectangle {
        id: backgroundRect

        anchors.fill: parent

        color: ui.theme.backgroundPrimaryColor
        radius: 8

        border.color: "lightgray"
        border.width: 1

        opacity: 0.95

        Rectangle {
            id: progressBar

            anchors.left: parent.left
            anchors.bottom: parent.bottom
            anchors.leftMargin: 2
            anchors.bottomMargin: 1

            visible: root.progress > 0 && !root.showProgressInfo

            height: 4
            width: (parent.width - 3) * (root.progress / 100)

            color: ui.theme.accentColor

            bottomLeftRadius: parent.radius - parent.border.width
        }
    }

    Row {
        id: mainContainer

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top

        anchors.topMargin: root.verticalMargin
        anchors.bottomMargin: root.verticalMargin
        anchors.rightMargin: root.rightMargin
        anchors.leftMargin: root.leftMargin
        spacing: root.iconContentSpacing

        StyledIconLabel {
            id: iconLabel

            width: root.iconSize

            iconCode: root.iconCode
        }

        Column {
            width: parent.width - iconLabel.width - mainContainer.spacing - (root.dismissable ? dismissButton.width + mainContainer.spacing : 0)
            spacing: root.textContentSpacing

            Column {
                id: textColumn

                width: parent.width
                spacing: root.titleMessageSpacing

                StyledTextLabel {
                    id: titleLabel

                    width: parent.width
                    wrapMode: Text.WordWrap
                    horizontalAlignment: Text.AlignLeft

                    font.pixelSize: root.titlePixelSize
                    font.bold: true

                    text: root.title
                }

                StyledTextLabel {
                    id: messageLabel

                    width: parent.width
                    wrapMode: Text.Wrap
                    horizontalAlignment: Text.AlignLeft
                    textFormat: Text.PlainText

                    font.pixelSize: root.messagePixelSize
                    font.bold: false

                    visible: root.message.length > 0

                    text: root.message
                }
            }

            ToastProgressBar {
                id: progressBarItem

                anchors.left: parent.left
                anchors.right: parent.right

                visible: root.showProgressInfo

                progress: root.progress
                timeElapsed: root.timeElapsed
            }

            Row {
                id: actionsRow

                width: parent.width
                spacing: root.actionButtonsSpacing

                Repeater {
                    model: root.actions.length

                    FlatButton {
                        text: root.actions[index].text
                        height: root.actionButtonHeight
                        minWidth: 0
                        margins: root.actionButtonMargins
                        onClicked: {
                            root.actionTriggered(root.actions[index].text);
                        }
                    }
                }
            }
        }

        FlatButton {
            id: dismissButton

            visible: root.dismissable
            width: root.dismissButtonSize
            height: width

            icon: IconCode.CLOSE_X_ROUNDED
            transparent: true

            onClicked: {
                dismissed()
            }
        }
    }
}
