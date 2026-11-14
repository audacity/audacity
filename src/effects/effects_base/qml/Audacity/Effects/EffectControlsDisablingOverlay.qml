import QtQuick
import Audacity.Effects

/**
 * For third-part effects, the only way we can safely block interaction with the UI is by placing an overlay window.
 *
 * With Qt 6.9.1, this doesn't work on Windows for built-in effects, who use QML-drawn controls. The reason is unknown at the time of writing.
 * We therefore use a simple rectangle component instead.
 */
Item {
    id: root

    required property int effectFamily

    WindowContainer {
        visible: root.visible && root.effectFamily !== EffectFamily.Builtin
        anchors.fill: parent

        window: Window {
            color: ui.theme.extra["effect_controls_disabling_overlay_color"]
        }
    }

    Rectangle {
        visible: root.visible && root.effectFamily === EffectFamily.Builtin
        anchors.fill: parent

        color: ui.theme.extra["effect_controls_disabling_overlay_color"]

        MouseArea {
            id: interceptingMouseArea
            anchors.fill: parent
            hoverEnabled: true
        }
    }
}
