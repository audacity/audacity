/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.Ui
import Muse.UiComponents

MenuButton {
    id: root

    property color backgroundColor: ui.theme.backgroundQuarternaryColor
    property Border border: Border{}

    icon: IconCode.SMALL_ARROW_DOWN
    iconColor: ui.theme.fontSecondaryColor

    menuAnchorItem: ui.rootItem

    navigation.name: "ArrowMenuButton"

    backgroundItem: RoundedRectangle {
        id: background

        topRightRadius: 3
        bottomRightRadius: 3

        color: root.backgroundColor
        border: root.border

        NavigationFocusBorder {
            navigationCtrl: root.navigation
            drawOutsideParent: !root.drawFocusBorderInsideRect
        }

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
