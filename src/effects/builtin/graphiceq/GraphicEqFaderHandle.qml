import QtQuick 2.15

Rectangle {
  width: 16
  height: 32
  radius: 2
  color: "white"
  border.color: "darkgray"
  border.width: 1

  Rectangle {
    width: 10
    height: 1
    color: "black"
    y: 16
    anchors.horizontalCenter: parent.horizontalCenter
  }
}