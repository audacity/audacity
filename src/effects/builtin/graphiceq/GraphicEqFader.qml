import QtQuick 2.15
import Audacity.ProjectScene

Item {
    id: root
    property double min: 0
    property double max: 0
    property double value: 0
    signal newValueRequested(double newValue)
    width: eqFaderHandle.width

    QtObject {
        id: prv
        property bool dragActive: false
    }

    Rectangle {
        id: faderTrack
        width: 4
        height: root.height
        color: ui.theme.isDark ? "#191F24" : "#B8BBCC"
        anchors.horizontalCenter: parent.horizontalCenter

        GraphicEqFaderHandle {
            id: eqFaderHandle
            anchors.horizontalCenter: parent.horizontalCenter
            y: faderTrack.height * (1 - (value - min) / (max - min)) - eqFaderHandle.height / 2

            VolumeTooltip {
                id: tooltip
                volume: root.value
            }

            MouseArea {
                anchors.fill: parent
                drag.target: eqFaderHandle
                drag.minimumY: -eqFaderHandle.height / 2
                drag.maximumY: faderTrack.height - eqFaderHandle.height / 2
                hoverEnabled: true

                onPositionChanged: {
                    const newValue = min + (max - min) * (1 - (eqFaderHandle.y + eqFaderHandle.height / 2) / faderTrack.height)
                    newValueRequested(Math.max(min, Math.min(max, newValue)))
                }

                onDoubleClicked: {
                    newValueRequested(0)
                }

                onPressed: {
                    prv.dragActive = true
                    tooltip.show(true)
                }

                onReleased: {
                    prv.dragActive = false
                    if (!containsMouse) {
                        tooltip.hide(true)
                    }
                }

                onEntered: {
                    tooltip.show()
                }

                onExited: {
                    if (!prv.dragActive) {
                        tooltip.hide(true)
                    }
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
