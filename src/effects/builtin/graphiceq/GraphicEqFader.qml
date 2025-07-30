import QtQuick 2.15

Item {
  id: root

  property double min: 0
  property double max: 0
  property double value: 0

  signal newValueRequested(double newValue)

  width: eqFaderHandle.width

  Rectangle {
    id: faderTrack
    width: 2
    height: root.height
    color: "#C0C5CE"
    anchors.horizontalCenter: parent.horizontalCenter

    GraphicEqFaderHandle {
      id: eqFaderHandle
      anchors.horizontalCenter: parent.horizontalCenter

      y: faderTrack.height * (1 - (value - min) / (max - min)) - eqFaderHandle.height / 2

      MouseArea {
        anchors.fill: parent
        drag.target: eqFaderHandle
        drag.minimumY: -eqFaderHandle.height / 2
        drag.maximumY: faderTrack.height - eqFaderHandle.height / 2

        onPositionChanged: {
          const newValue = min + (max - min) * (1 - (eqFaderHandle.y + eqFaderHandle.height / 2) / faderTrack.height)
          newValueRequested(Math.max(min, Math.min(max, newValue)))
        }

        onDoubleClicked: {
          newValueRequested(0)
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
