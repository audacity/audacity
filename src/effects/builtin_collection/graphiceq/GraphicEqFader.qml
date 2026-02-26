import QtQuick 2.15
import Audacity.ProjectScene

import Muse.UiComponents

Item {
    id: root

    property double min: 0
    property double max: 0
    property double value: 0
    property double stepSize: 1
    property alias handle: eqFaderHandle
    property alias navigation: navCtrl

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

    NavigationControl {
        id: navCtrl
        name: root.objectName !== "" ? root.objectName : "GraphicEqFader"
        enabled: root.enabled && root.visible

        accessible.role: MUAccessible.Range
        accessible.visualItem: root
        accessible.value: root.value
        accessible.minimumValue: root.min
        accessible.maximumValue: root.max
        accessible.stepSize: root.stepSize

        onNavigationEvent: function(event) {
            switch (event.type) {
            case NavigationEvent.Up:
                root.requestNewValue(Math.min(root.max, root.value + root.stepSize))
                event.accepted = true
                break
            case NavigationEvent.Down:
                root.requestNewValue(Math.max(root.min, root.value - root.stepSize))
                event.accepted = true
                break
            }
        }
    }

    Rectangle {
        id: faderTrack

        width: 4
        height: root.height
        anchors.horizontalCenter: parent.horizontalCenter

        color: ui.theme.extra["graphic_eq_fader_track_color"]

        GraphicEqFaderHandle {
            id: eqFaderHandle

            anchors.horizontalCenter: parent.horizontalCenter
            y: faderTrack.height * (1 - (value - min) / (max - min)) - eqFaderHandle.height / 2

            NavigationFocusBorder {
                anchors.fill: parent
                navigationCtrl: navCtrl
            }

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
