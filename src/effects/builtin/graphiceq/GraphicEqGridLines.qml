import QtQuick 2.15
import Muse.Ui
import Muse.UiComponents 1.0

Item {
  id: root

  // In
  property int min
  property int max
  property int lineWidth
  property int fontSize

  // Out
  property int gridlineHorizontalCenter: prv.labelWidth + prv.labelLineSpacing + root.lineWidth / 2

  width: prv.labelWidth + prv.labelLineSpacing + root.lineWidth

  QtObject {
    id: prv
    readonly property int labelLineSpacing: 8
    readonly property int labelWidth: 16
  }

  Repeater {
    model: {
      let lines = [];
      let start = Math.ceil(root.min / 6) * 6;
      let end = Math.floor(root.max / 6) * 6;
      for (let v = start; v <= end; v += 6) {
        lines.push(v);
      }
      return lines;
    }
    delegate: Item {
      width: root.lineWidth
      height: 1

      StyledTextLabel {
        id: label
        width: prv.labelWidth
        y: line.y - 6
        horizontalAlignment: Text.AlignRight
        text: modelData
        font.pixelSize: root.fontSize
      }

      Rectangle {
        id: line
        anchors.left: label.right
        anchors.leftMargin: prv.labelLineSpacing
        width: root.lineWidth
        height: 1
        y: root.height * (1 - (modelData - root.min) / (root.max - root.min))
        color: ui.theme.strokeColor
      }
    }
  }
}
