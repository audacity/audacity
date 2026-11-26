import QtQuick

import Muse.Ui

import Audacity.ProjectScene

//! What a clip nobody interacts with shows, with a few objects instead of the many of a full ClipItem
Rectangle {
    id: root

    property alias context: waveView.context
    property alias clipKey: waveView.clipKey
    property alias clipTime: waveView.clipTime
    property string title: ""
    property color clipColor: ui.theme.extra["clip_color_1"]
    property color clipSelectedColor: ui.theme.extra["clip_selected_color_1"]
    property color clipTimeSelectionColor: "transparent"
    property int currentClipStyle: ClipStyle.COLORFUL
    property int headerHeight: 0
    property bool collapsed: false
    property bool isAudible: true
    property bool isDataSelected: false
    property bool showChannelSplitter: false
    property real channelHeightRatio: 0.5
    property bool isLinear: false
    property real dbRange: -60.0
    property var displayBounds: ({
            "min": -1.0,
            "max": 1.0
        })
    property real selectionStart: 0
    property real selectionWidth: 0
    property real leftVisibleMargin: 0
    property int pitch: 0
    property bool isPitchModified: false
    property int speedPercentage: 0
    property bool isSpeedModified: false

    readonly property bool isLightView: true

    //! How the pitch and speed buttons of a full ClipItem look
    component PropertyLabel: Row {
        property alias icon: iconLabel.text
        property alias text: textLabel.text

        height: 16
        leftPadding: 4
        rightPadding: 4
        spacing: 2

        Text {
            id: iconLabel

            width: 8
            anchors.verticalCenter: parent.verticalCenter

            horizontalAlignment: Text.AlignHCenter
            color: ui.theme.extra["black_color"]
            font.family: ui.theme.iconsFont.family
            font.pixelSize: 12
        }

        Text {
            id: textLabel

            anchors.verticalCenter: parent.verticalCenter

            color: ui.theme.extra["black_color"]
            font: ui.theme.bodyFont
        }
    }

    function updateView() {
        waveView.scheduleRepaint()
    }

    radius: 4
    color: root.clipColor
    opacity: root.isAudible ? 1.0 : 0.3

    Rectangle {
        id: header

        width: parent.width
        height: root.headerHeight

        topLeftRadius: root.radius
        topRightRadius: root.radius

        visible: !root.collapsed
        color: root.currentClipStyle == ClipStyle.COLORFUL ? root.clipColor : ui.theme.extra["classic_clip_header_color"]

        Rectangle {
            x: waveView.drawnSelectionValid ? waveView.drawnSelectionStart : root.selectionStart
            width: waveView.drawnSelectionValid ? waveView.drawnSelectionEnd - waveView.drawnSelectionStart : Math.min(root.selectionWidth, header.width)
            height: parent.height

            topLeftRadius: x <= 0 ? root.radius : 0
            topRightRadius: x + width >= header.width ? root.radius : 0

            color: root.clipTimeSelectionColor
            visible: root.isDataSelected && root.currentClipStyle == ClipStyle.COLORFUL
        }

        Text {
            anchors.verticalCenter: parent.verticalCenter
            x: root.leftVisibleMargin + 4
            width: Math.max(0, buttonsRow.x - 8 - x)

            text: root.title
            elide: Text.ElideRight
            color: ui.theme.extra["black_color"]
            font: ui.theme.bodyFont
        }

        Row {
            id: buttonsRow

            anchors.right: parent.right
            anchors.rightMargin: 4
            anchors.verticalCenter: parent.verticalCenter

            spacing: 2

            Loader {
                id: pitchLabel

                anchors.verticalCenter: parent.verticalCenter

                active: root.isPitchModified
                visible: active && header.width > (60 + pitchLabel.implicitWidth + speedLabel.implicitWidth + menuIcon.width)

                sourceComponent: PropertyLabel {
                    icon: String.fromCharCode(root.pitch > 0 ? IconCode.ARROW_UP : IconCode.ARROW_DOWN)
                    text: {
                        let semis = Math.trunc(root.pitch / 100)
                        let cents = Math.abs(root.pitch % 100).toString().padStart(2, "0")
                        return semis + (cents > "00" ? "." + cents : "")
                    }
                }
            }

            Loader {
                id: speedLabel

                anchors.verticalCenter: parent.verticalCenter

                active: root.isSpeedModified
                visible: active && header.width > (60 + speedLabel.implicitWidth + menuIcon.width)

                sourceComponent: PropertyLabel {
                    icon: String.fromCharCode(IconCode.CLOCK)
                    text: root.speedPercentage + "%"
                }
            }

            Text {
                id: menuIcon

                width: 16
                height: 16
                anchors.verticalCenter: parent.verticalCenter

                horizontalAlignment: Text.AlignHCenter
                verticalAlignment: Text.AlignVCenter
                text: String.fromCharCode(IconCode.MENU_THREE_DOTS)
                color: ui.theme.extra["black_color"]
                font: ui.theme.iconsFont
                visible: header.width > 60 + 16
            }
        }
    }

    WaveView {
        id: waveView

        y: header.visible ? header.height : 0
        width: parent.width
        height: parent.height - y

        clipColor: root.clipColor
        clipSelectedColor: root.clipSelectedColor
        clipSelected: false

        channelHeightRatio: root.showChannelSplitter ? root.channelHeightRatio : 1
        cornerRadius: root.radius
        roundTopCorners: !header.visible
        isLinear: root.isLinear
        dbRange: root.dbRange
        displayBounds: root.displayBounds

        Rectangle {
            y: Math.round(root.channelHeightRatio * parent.height) - 1
            width: parent.width
            height: 1

            color: ui.theme.extra["black_color"]
            opacity: 0.05
            visible: root.showChannelSplitter
        }
    }

    Rectangle {
        anchors.fill: parent

        color: "transparent"
        border.width: 1
        border.color: ui.theme.extra["black_color"]
        radius: root.radius
    }
}
