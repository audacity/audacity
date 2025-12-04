import QtQuick

import Muse.UiComponents

Rectangle {

    id: root

    property color clipColor: "#677CE4"
    property bool collapsed: false

    property alias title: titleLabel.text
    property real desiredWidth: -1

    width: desiredWidth <= 0 ? titleLabel.implicitWidth + 20 : desiredWidth

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

        Rectangle {
            id: clipBodyGradient

            x: parent.width - 20
            height: root.height - header.height - 1
            width: 50

            anchors.bottom: parent.bottom

            visible: desiredWidth <= 0

            gradient: Gradient {
                orientation: Qt.Horizontal
                GradientStop { position: 0.0; color: "#00FFFFFF" }
                GradientStop { position: 0.4; color: ui.blendColors(ui.blendColors("transparent", "#FFFFFF", 0.3), ui.blendColors("transparent", root.clipColor, 0.4), 0.5)}
                // /*ui.blendColors("#FFFFFF", root.clipColor, 0.5)*//*root.clipColor;*/
                GradientStop { position: 1.0; color: "#00FFFFFF" }
            }
        }

        Rectangle {
            id: bottomBorderGradient

            x: parent.width - 20
            y: parent.height - 1

            height: 1
            width: 50

            visible: desiredWidth <= 0

            gradient: Gradient {
                orientation: Qt.Horizontal
                GradientStop { position: 0.0; color: "#00FFFFFF" }
                GradientStop { position: 0.4; color: "#FFFFFFFF" }
                GradientStop { position: 1.0; color: "#00FFFFFF" }
            }
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

        Rectangle {
            id: headerGradient

            x: parent.width - 20
            width: 50
            height: header.height - 1

            visible: desiredWidth <= 0

            gradient: Gradient {
                orientation: Qt.Horizontal
                GradientStop { position: 0.0; color: "#00FFFFFF" }
                GradientStop { position: 0.4; color: ui.blendColors("#FFFFFF", root.clipColor, 0.3) }
                GradientStop { position: 1.0; color: "#00FFFFFF" }
            }
        }
    }
}
