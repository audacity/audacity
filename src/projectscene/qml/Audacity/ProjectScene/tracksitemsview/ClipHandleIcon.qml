import QtQuick

import Muse.UiComponents

// Icon of a clip handle, growing while the handle is hovered
StyledIconLabel {
    id: root

    property bool hovered: false

    property int iconSize: 17
    property int hoveredIconSize: 22

    font.pixelSize: root.iconSize

    states: [
        State {
            name: "HOVERED"
            when: root.hovered
            PropertyChanges {
                target: root
                font.pixelSize: root.hoveredIconSize
            }
        }
    ]

    transitions: [
        Transition {
            from: "*"
            to: "*"
            reversible: true

            PropertyAnimation {
                target: root
                properties: "font.pixelSize"
                duration: 100
                easing.type: Easing.Linear
            }
        }
    ]
}
