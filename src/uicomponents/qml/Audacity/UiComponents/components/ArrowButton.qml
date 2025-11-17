/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.Ui
import Muse.UiComponents

FlatButton {
    id: root

    property bool isDown: true

    property real topRightRadius: 3
    property real bottomRightRadius: 3

    icon: isDown ? IconCode.SMALL_ARROW_DOWN : IconCode.SMALL_ARROW_UP
    iconColor: ui.theme.fontSecondaryColor

    backgroundItem: RoundedRectangle {
        id: background

        topRightRadius: root.topRightRadius
        bottomRightRadius: root.bottomRightRadius

        color: ui.theme.backgroundQuarternaryColor

        states: [
            State {
                name: "PRESSED"
                when: root.mouseArea.pressed

                PropertyChanges {
                    target: background
                    opacity: ui.theme.buttonOpacityHit
                }
            },

            State {
                name: "HOVERED"
                when: root.mouseArea.containsMouse && !root.mouseArea.pressed

                PropertyChanges {
                    target: background
                    opacity: ui.theme.buttonOpacityHover
                }
            }
        ]
    }

    mouseArea.onPressAndHold: { continuousTimer.running = true }
    mouseArea.onReleased: { continuousTimer.running = false }

    Timer {
        id: continuousTimer

        interval: 100
        repeat: true

        onTriggered: {
            root.clicked(null)
        }
    }
}
