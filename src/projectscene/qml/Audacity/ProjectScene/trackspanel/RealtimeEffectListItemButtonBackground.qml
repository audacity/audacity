import QtQuick

import Muse.Ui

Rectangle {
    id: root

    property var mouseArea: null

    color: ui.theme.backgroundPrimaryColor
    radius: 3
    border.width: 1
    border.color: ui.theme.strokeColor

    states: [
        State {
            name: "HOVERED"
            when: mouseArea.containsMouse && !mouseArea.pressed

            PropertyChanges {
                target: root
                color: ui.theme.buttonColor
                opacity: 0.4
            }
        },

        State {
            name: "PRESSED"
            when: mouseArea.pressed

            PropertyChanges {
                target: root
                color: ui.theme.buttonColor
                opacity: 0.7
            }
        }
    ]
}
