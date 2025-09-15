/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.9
import QtQuick.Window 2.2
import QtQuick.Controls 2.2
import Audacity.BuiltinEffects 1.0

Window {
    visible: true
    width: 640
    height: 400
    title: "DynamicsPanel draft"

    Rectangle {
        id: container

        width: 600
        height: timeline.height + buttonRow.height + column.spacing + column.topPadding + column.bottomPadding
        anchors.centerIn: parent
        border.color: "darkgrey"
        border.width: 1

        Column {
            id: column

            anchors.fill: parent
            padding: 16
            topPadding: 12
            spacing: 8

            DynamicsPanel {
                id: timeline
                width: container.width - 32
                instanceId: 42
                playState: Stopwatch.Playing
            }

            Row {
                id: buttonRow

                spacing: 8
                anchors.horizontalCenter: parent.horizontalCenter

                Repeater {
                    model: [Stopwatch.Playing, Stopwatch.Paused, Stopwatch.Stopped]

                    delegate: Button {
                        text: modelData === Stopwatch.Playing ? "play" : modelData === Stopwatch.Paused ? "pause" : "stop"
                        onClicked: timeline.playState = modelData
                    }
                }
            }
        }
    }
}
