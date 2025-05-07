import QtQuick

import Muse.Ui
import Muse.UiComponents

Rectangle {
    id: root

    property var mouseArea: null
    property NavigationControl navigationCtrl: null

    color: ui.theme.backgroundPrimaryColor
    radius: 3
    border.width: 1
    border.color: ui.theme.strokeColor

    NavigationFocusBorder {
        navigationCtrl: root.navigationCtrl
    }

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
