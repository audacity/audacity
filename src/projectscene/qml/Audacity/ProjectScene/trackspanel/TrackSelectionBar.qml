import QtQuick
import QtQuick.Layouts

Rectangle {
    id: root

    property bool isHovered: false
    property bool isSelected: false

    Layout.fillHeight: true
    Layout.preferredWidth: 6

    states: [
        State {
            name: "Default"
            when: !isSelected && !isHovered
            PropertyChanges {
                target: root
                color: ui.theme.strokeColor
                opacity: 1
            }
        },
        State {
            name: "Hovered"
            when: !isSelected && isHovered
            PropertyChanges {
                target: root
                color: ui.theme.accentColor
                opacity: 0.5
            }
        },
        State {
            name: "Selected"
            when: isSelected
            PropertyChanges {
                target: root
                color: ui.theme.accentColor
                opacity: 1
            }
        }
    ]
}
