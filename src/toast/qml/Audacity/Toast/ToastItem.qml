import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

Item {
    id: root

    property int iconCode: IconCode.NONE
    property string title: ""
    property string message: ""
    property bool dismissable: true
    property int progress: 0
    property var actions: []

    property int margins: 12
    property int spacing: 12
    property int dismissButtonSize: 16
    property int iconSize: 16
    property int titlePixelSize: 12
    property int messagePixelSize: 12
    property int actionButtonHeight: 24
    property int actionButtonMargins: 8
    property int actionButtonsSpacing: 8

    signal dismissed()
    signal actionTriggered(string actionStr)

    width: 360
    height: implicitHeight

    implicitHeight: contentContainer.childrenRect.height + 24

    Rectangle {
        id: backgroundRect

        anchors.fill: parent

        color: ui.theme.backgroundPrimaryColor
        radius: 8

        border.color: "lightgray"
        border.width: 1

        opacity: 0.95
    }

    ProgressBar {
        id: progressBar

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom

        height: 4

        from: 0
        to: 100

        visible: root.progress > 0
        value: 100 - root.progress

        onValueChanged: {
            if (value <= 0) {
                root.dismissed();
            }
        }
    }

    Row {
        id: contentContainer

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top

        anchors.margins: root.margins
        spacing: root.spacing

        StyledIconLabel {
            id: iconLabel

            width: root.iconSize

            iconCode: root.iconCode
        }

        Column {
            id: textColumn

            width: parent.width - iconLabel.width - contentContainer.spacing - (root.dismissable ? dismissButton.width + contentContainer.spacing : 0)
            spacing: 4

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
                wrapMode: Text.WordWrap
                horizontalAlignment: Text.AlignLeft

                font.pixelSize: root.messagePixelSize
                font.bold: false

                text: root.message
            }

            Item {
                id: spacerItem

                visible: root.actions && root.actions.length > 0

                width: parent.width
                height: 4
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
