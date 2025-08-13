import QtQuick 2.15
import Audacity.ProjectScene

Item {
    id: root

    property double min: 0
    property double max: 0
    property double value: 0
    property alias handle: eqFaderHandle

    function showTooltip() {
        // The user just clicked the effect's window.
        // If the tooltip has already appeared due to `onEntered`,
        // the tooltip gets hidden, probably because the effect's
        // window comes into focus. This seems to be a bug in the
        // framework.
        // I also tried Qt.callLater(function() { tooltip.forceActiveFocus() }),
        // without success.
        // For now, we close the tooltip and re-opening. It's better than no
        // tooltip at all.
        if (tooltip.isOpened) {
            tooltip.hide(true)
        }

        tooltip.show(true)
    }

    function hideTooltip() {
        tooltip.hide(true)
    }

    width: eqFaderHandle.width

    Rectangle {
        id: faderTrack

        width: 4
        height: root.height
        anchors.horizontalCenter: parent.horizontalCenter

        color: ui.theme.isDark ? "#191F24" : "#B8BBCC"

        GraphicEqFaderHandle {
            id: eqFaderHandle

            anchors.horizontalCenter: parent.horizontalCenter
            y: faderTrack.height * (1 - (value - min) / (max - min)) - eqFaderHandle.height / 2

            VolumeTooltip {
                id: tooltip
                volume: root.value

                // A rather long showDelay, so that the user falls less often
                // in the situation described above - if the tooltip hasn't yet
                // appear, we won't have to close it and reopen it when the user
                // presses the fader and we won't get that ugly blinking.
                showDelay: 1000
            }

            MouseArea {
                anchors.fill: parent
                hoverEnabled: true

                onPressed: function(mouse) {
                    mouse.accepted = false
                }

                onEntered: {
                    tooltip.show()
                }

                onExited: {
                    tooltip.hide()
                }
            }
        }

        // QtGraphicalEffects was removed from Qt 6.
        // This would use it from Qt5Compat.GraphicalEffects, but it looks ugly,
        // maybe because `samples` seems not to be a property anymore.
        // MultiEffect is recommended instead, but alledgedly introduced in Qt 6.5 (were's still at 6.2.4 at the time of writing).
        // DropShadow {
        //   anchors.fill: eqFaderHandle
        //   horizontalOffset: 3
        //   verticalOffset: 3
        //   radius: 8.0
        //   color: "#80000000"
        //   source: eqFaderHandle
        // }
    }
}
