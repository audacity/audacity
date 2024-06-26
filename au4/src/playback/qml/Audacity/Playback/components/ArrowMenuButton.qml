/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.Ui
import Muse.UiComponents

MenuButton {
    id: menuBtn

    icon: IconCode.SMALL_ARROW_DOWN
    iconColor: ui.theme.fontSecondaryColor

    menuAnchorItem: ui.rootItem

    backgroundItem: RoundedRectangle {
        id: background

        topRightRadius: 3
        bottomRightRadius: 3

        color: ui.theme.backgroundQuarternaryColor

        states: [
            State {
                name: "PRESSED"
                when: menuBtn.mouseArea.pressed

                PropertyChanges {
                    target: background
                    opacity: ui.theme.buttonOpacityHit
                }
            },

            State {
                name: "HOVERED"
                when: menuBtn.mouseArea.containsMouse && !menuBtn.mouseArea.pressed

                PropertyChanges {
                    target: background
                    opacity: ui.theme.buttonOpacityHover
                }
            }
        ]
    }
}
