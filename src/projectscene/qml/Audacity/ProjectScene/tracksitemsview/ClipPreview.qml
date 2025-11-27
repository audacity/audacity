import QtQuick

import Muse.UiComponents

Rectangle {

    id: root

    property color clipColor: "#677CE4"
    property bool collapsed: false

    property alias title: titleLabel.text

    radius: 4
    border.width: 1
    border.color: "#000000"

    color: "transparent"

    Rectangle {
        id: borderRect

        anchors.fill: parent
        anchors.margins: 1
        color: "transparent"

        border.width:  1
        border.color: "#FFFFFF"
        radius: 4

        Rectangle {
            id: contentBackground

            anchors.fill: parent
            anchors.margins: 1
            color: "#FFFFFF"
            opacity: 0.3

            radius: 4
        }

        Rectangle {
            id: contentForeground

            anchors.fill: parent
            anchors.margins: 1
            color: root.clipColor
            opacity: 0.4

            radius: 4
        }

    }

    RoundedRectangle {
        id: header
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.margins: 1

        border.width: 2

        topLeftRadius: root.radius
        topRightRadius: root.radius

        height: 20

        color: ui.blendColors("#FFFFFF", root.clipColor, 0.3)

        visible: !root.collapsed

        StyledTextLabel {
            id: titleLabel

            anchors.fill: parent
            anchors.leftMargin: 4
            anchors.rightMargin: 8
            horizontalAlignment: Qt.AlignLeft

            color: "#000000"
        }
    }
}
