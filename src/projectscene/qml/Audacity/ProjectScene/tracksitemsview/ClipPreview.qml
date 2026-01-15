import QtQuick

import Muse.UiComponents

import Audacity.UiComponents
import Audacity.ProjectScene


Rectangle {

    id: root

    property color clipColor: null
    property bool collapsed: false

    property alias title: titleLabel.text
    property real desiredWidth: -1

    property int currentClipStyle: ClipStyle.COLORFUL
    property color classicThemeBackground: ui.theme.extra["classic_clip_background_color"]
    property color classicThemeHeader: ui.theme.extra["classic_clip_header_color"]

    property color classicThemeGradient: ui.blendColors(ui.blendColors("transparent", "white", 0.3), ui.blendColors("transparent", root.classicThemeBackground, 0.4), 0.5)
    property color colorfulThemeGradient: ui.blendColors(ui.blendColors("transparent", "white", 0.3), ui.blendColors("transparent", root.clipColor, 0.4), 0.5)

    width: desiredWidth <= 0 ? titleLabel.implicitWidth + 20 : desiredWidth

    radius: 4
    border.width: 1
    border.color: "black"

    color: "transparent"

    Rectangle {
        id: borderRect

        anchors.fill: parent
        anchors.margins: 1
        color: "transparent"

        border.width:  1
        border.color: "white"
        radius: 4

        Rectangle {
            id: contentBackground

            anchors.fill: parent
            anchors.margins: 1
            color: "white"
            opacity: 0.3

            radius: 4
        }

        Rectangle {
            id: contentForeground

            anchors.fill: parent
            anchors.margins: 1
            color: root.currentClipStyle == ClipStyle.COLORFUL ? root.clipColor : root.classicThemeBackground
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
                GradientStop { position: 0.4; color: root.currentClipStyle == ClipStyle.COLORFUL ? colorfulThemeGradient : classicThemeGradient}
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

        color: root.currentClipStyle == ClipStyle.COLORFUL ? ui.blendColors("white", root.clipColor, 0.3) : ui.blendColors("white", root.classicThemeHeader, 0.3)

        visible: !root.collapsed

        StyledTextLabel {
            id: titleLabel

            anchors.fill: parent
            anchors.leftMargin: 4
            anchors.rightMargin: 8
            horizontalAlignment: Qt.AlignLeft

            color: "black"
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
                GradientStop { position: 0.4; color: root.currentClipStyle == ClipStyle.COLORFUL ? ui.blendColors("white", root.clipColor, 0.3) : ui.blendColors("white", root.classicThemeHeader, 0.3)}
                GradientStop { position: 1.0; color: "#00FFFFFF" }
            }
        }
    }
}
