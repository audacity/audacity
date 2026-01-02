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
    property int autoDismissTimeout: 5000
    property var actions: null

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
        value: 100

        visible: root.autoDismissTimeout > 0

        Timer {
            id: progressBarTimer

            interval: 50
            running: root.autoDismissTimeout > 0
            repeat: true
            onTriggered: {
                progressBar.value -= 50 / root.autoDismissTimeout * 100;
 
                if (progressBar.value <= 0) {
                    progressBar.visible = false;
                    progressBarTimer.stop();
                    root.dismissed();
                }
            }
        }
    }

    Row {
        id: contentContainer

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top

        anchors.margins: 12
        spacing: 12

        StyledIconLabel {
            id: iconLabel

            width: 16

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

                font.pixelSize: 12
                font.bold: true

                text: root.title
            }

            StyledTextLabel {
                id: messageLabel

                width: parent.width
                wrapMode: Text.WordWrap
                horizontalAlignment: Text.AlignLeft

                font.pixelSize: 12
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
                spacing: 8

                Repeater {
                    model: root.actions.length

                    FlatButton {
                        text: root.actions[index].text
                        height: 24
                        minWidth: 0
                        margins: 8
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
            width: 16
            height: 16

            icon: IconCode.CLOSE_X_ROUNDED
            transparent: true

            onClicked: {
                dismissed()
            }
        }
    }
}