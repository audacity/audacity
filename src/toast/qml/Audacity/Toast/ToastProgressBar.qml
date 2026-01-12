/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

Item {
    id: root

    height: mainContainer.height

    property int progress: 0
    property int timeElapsed: 0

    property int contentSpacing: 8
    property int contentMargins: 0
    property int messagePixelSize: 14
    property int progressBarHeight: 8

    onTimeElapsedChanged: {
        progressTextHelper.updateText()
    }

    onProgressChanged: {
        progressTextHelper.updateText()
    }

    QtObject {
        id: progressTextHelper

        property string text: ""

        function updateText() {
            if (root.progress <= 0 || root.timeElapsed <= 0) {
                text = ""
                return
            }

            const SECONDS_IN_HOUR = 3600;
            const SECONDS_IN_MINUTE = 60;

            const estimatedTotalTime = root.timeElapsed / (root.progress / 100.0)
            const remainingSeconds = Math.max(0, estimatedTotalTime - root.timeElapsed)

            if (remainingSeconds >= SECONDS_IN_HOUR) {
                const hours = Math.round(remainingSeconds / SECONDS_IN_HOUR)
                text = hours === 1 ? qsTrc("toast", "1 hour remaining") : hours + qsTrc("toast", " hours remaining")
            } else if (remainingSeconds >= SECONDS_IN_MINUTE) {
                const minutes = Math.round(remainingSeconds / SECONDS_IN_MINUTE)
                text = minutes === 1 ? qsTrc("toast", "1 minute remaining") : minutes + qsTrc("toast", " minutes remaining")
            } else {
                const seconds = Math.round(remainingSeconds)
                text = seconds === 1 ? qsTrc("toast", "1 second remaining") : seconds + qsTrc("toast", " seconds remaining")
            }
        }
    }

    Column {
        id: mainContainer
    
        anchors.left: parent.left
        anchors.right: parent.right

        height: progressBar.height + progressText.height + root.contentSpacing

        anchors.margins: root.contentMargins

        spacing: root.contentSpacing

        ProgressBar {
            id: progressBar

            width: parent.width
            height: root.progressBarHeight
            value: root.progress / 100.0
        }

        Item{
            id: progressText

            width: parent.width
            height: leftLabel.height

            StyledTextLabel {
                id: leftLabel

                anchors.left: parent.left
                horizontalAlignment: Text.AlignLeft

                font.pixelSize: root.messagePixelSize

                text: progressTextHelper.text
            }

            StyledTextLabel {
                id: rightLabel

                anchors.right: parent.right
                horizontalAlignment: Text.AlignRight

                font.pixelSize: root.messagePixelSize

                text: Math.min(Math.max(root.progress, 0), 100) + "%"
            }
        }
    }
}