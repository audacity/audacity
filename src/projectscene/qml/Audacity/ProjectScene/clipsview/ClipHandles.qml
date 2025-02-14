import QtQuick

import Muse.Ui
import Muse.UiComponents

Item {
    id: root

    property var canvas: null
    property bool handlesHovered: false
    property bool handlesVisible: false

    property int animationDuration: 100

    property bool debugRectsVisible: false

    // mouse position event is not propagated on overlapping mouse areas
    // so we are handling it manually
    signal clipHandlesMousePositionChanged(real x, real y)

    property var trimStartPos

    signal clipStartEditRequested()
    signal clipEndEditRequested()

    signal trimLeftRequested(bool completed)
    signal trimRightRequested(bool completed)

    signal stretchLeftRequested(bool completed)
    signal stretchRightRequested(bool completed)

    //! NOTE: auto-scroll for trimming is triggered from clipslistmodel
    signal stopAutoScroll()

    Item {
        id: leftTrimHandle

        x: -24
        height: 32
        width: 36

        visible: handlesVisible

        Rectangle {
            anchors.fill: parent

            color: "transparent"
            border.width: 1
            border.color: "blue"

            visible: debugRectsVisible
        }


        StyledIconLabel {
            id: leftArrow
            anchors.verticalCenter: leftTrimHandle.verticalCenter
            anchors.left: leftTrimHandle.left
            // specific icon pivot
            anchors.leftMargin: -5

            iconCode: IconCode.SMALL_ARROW_LEFT
            font.pixelSize: 30
            color: "black"
            style: Text.Outline
            styleColor: "white"

            Rectangle {
                height: 12
                width: 12
                anchors.verticalCenter: leftArrow.verticalCenter
                anchors.horizontalCenter: leftArrow.horizontalCenter

                color: "transparent"
                border.color: "red"
                border.width: 1

                visible: debugRectsVisible
            }
        }

        MouseArea {
            id: leftTrimMa

            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.SizeHorCursor

            onPressed: function(e) {
                root.clipStartEditRequested()
            }

            onReleased: function(e) {
                root.trimLeftRequested(true)
                root.stopAutoScroll()

                // this needs to be always at the very end
                root.clipEndEditRequested()
            }

            onEntered: {
                handlesHovered = true
            }

            onExited: {
                if (!leftTimeMa.containsMouse) {
                    handlesHovered = false
                }
            }

            onPositionChanged: function(e) {
                clipHandlesMousePositionChanged(mouseX + leftTrimHandle.x, mouseY)

                if (pressed) {
                    root.trimLeftRequested(false)
                }
            }
        }
    }

    Item {
        id: rightTrimHandle

        x: parent.width - 12
        height: 32
        width: 36

        visible: handlesVisible

        Rectangle {
            anchors.fill: parent

            color: "transparent"
            border.width: 1
            border.color: "blue"

            visible: debugRectsVisible
        }

        StyledIconLabel {
            id: rightArrow
            anchors.verticalCenter: rightTrimHandle.verticalCenter
            anchors.right: rightTrimHandle.right
            // specific icon pivot
            anchors.rightMargin: -5

            iconCode: IconCode.SMALL_ARROW_RIGHT
            font.pixelSize: 30
            color: "black"
            style: Text.Outline
            styleColor: "white"

            Rectangle {
                height: 12
                width: 12
                anchors.verticalCenter: rightArrow.verticalCenter
                anchors.horizontalCenter: rightArrow.horizontalCenter
                color: "transparent"

                border.color: "red"
                border.width: 1

                visible: debugRectsVisible
            }
        }

        MouseArea {
            id: rightTrimMa

            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.SizeHorCursor

            onPressed: function(e) {
                root.clipStartEditRequested()
            }

            onReleased: function(e) {
                root.trimRightRequested(true)
                root.stopAutoScroll()

                // this needs to be always at the very end
                root.clipEndEditRequested()
            }

            onEntered: {
                handlesHovered = true
            }

            onExited: {
                if (!rightTimeMa.containsMouse) {
                    handlesHovered = false
                }
            }

            onPositionChanged: function(e) {
                clipHandlesMousePositionChanged(mouseX + rightTrimHandle.x, mouseY)

                if (pressed) {
                    root.trimRightRequested(false)
                }
            }
        }
    }

    Item {
        id: leftTimecode

        x: -24
        y: leftTrimHandle.height
        height: 32
        width: 36

        visible: handlesVisible

        Rectangle {
            anchors.fill: parent

            color: "transparent"
            border.width: 1
            border.color: "blue"

            visible: debugRectsVisible
        }

        Rectangle {
            id: leftClock

            width: leftClockIcon.font.pixelSize - 2
            height: leftClockIcon.font.pixelSize - 2
            radius: (leftClockIcon.font.pixelSize - 2) / 2
            anchors.verticalCenter: leftTimecode.verticalCenter
            anchors.left: leftTimecode.left
            anchors.leftMargin: 4

            color: "black"

            StyledIconLabel {
                id: leftClockIcon

                anchors.centerIn: parent

                iconCode: IconCode.CLOCK
                font.pixelSize: 14
                color: "white"

                Rectangle {
                    height: 12
                    width: 12
                    anchors.verticalCenter: leftClockIcon.verticalCenter
                    anchors.horizontalCenter: leftClockIcon.horizontalCenter
                    color: "transparent"

                    border.color: "red"
                    border.width: 1

                    visible: debugRectsVisible
                }
            }
        }

        MouseArea {
            id: leftTimeMa
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.SizeHorCursor

            onPressed: {
                root.clipStartEditRequested()
            }

            onReleased: {
                root.stretchLeftRequested(true)
                root.stopAutoScroll()

                // this needs to be always at the very end
                root.clipEndEditRequested()
            }

            onEntered: {
                handlesHovered = true
            }

            onExited: {
                if (!leftTrimMa.containsMouse && mouseY > 2) {
                    handlesHovered = false
                }
            }

            onPositionChanged: {
                clipHandlesMousePositionChanged(mouseX + leftTimecode.x, mouseY)

                if (pressed) {
                    root.stretchLeftRequested(false)
                }
            }
        }
    }

    Item {
        id: rightTimecode

        x: parent.width - 12
        y: rightTrimHandle.height
        height: 32
        width: 36

        visible: handlesVisible

        Rectangle {
            anchors.fill: parent

            color: "transparent"
            border.width: 1
            border.color: "blue"
            visible: debugRectsVisible
        }

        Rectangle {
            id: rightClock

            width: (rightClockIcon.font.pixelSize - 2)
            height: (rightClockIcon.font.pixelSize - 2)
            radius: (rightClockIcon.font.pixelSize - 2) / 2
            anchors.verticalCenter: rightTimecode.verticalCenter
            anchors.right: rightTimecode.right
            anchors.rightMargin: 4

            color: "black"

            StyledIconLabel {
                id: rightClockIcon

                anchors.centerIn: parent
                iconCode: IconCode.CLOCK
                font.pixelSize: 14
                color: "white"

                Rectangle {
                    height: 12
                    width: 12
                    anchors.verticalCenter: rightClockIcon.verticalCenter
                    anchors.horizontalCenter: rightClockIcon.horizontalCenter
                    color: "transparent"

                    border.color: "red"
                    border.width: 1
                    visible: debugRectsVisible
                }
            }
        }

        MouseArea {
            id: rightTimeMa
            anchors.fill: parent
            hoverEnabled: true
            cursorShape: Qt.SizeHorCursor

            onPressed: {
                root.clipStartEditRequested()
            }

            onReleased: {
                root.stretchRightRequested(true)
                root.stopAutoScroll()

                // this needs to be always at the very end
                root.clipEndEditRequested()
            }

            onEntered: {
                handlesHovered = true
            }

            onExited: {
                if (!rightTrimMa.containsMouse && mouseY > 2) {
                    handlesHovered = false
                }
            }

            onPositionChanged: {
                clipHandlesMousePositionChanged(mouseX + rightTimecode.x, mouseY)

                if (pressed) {
                    root.stretchRightRequested(false)
                }
            }
        }
    }

    state: "NORMAL"
    states: [
        State {
            name: "NORMAL"
            when: !leftTrimMa.containsMouse && !rightTrimMa.containsMouse && !leftTimeMa.containsMouse && !rightTimeMa.containsMouse
        },

        State {
            name: "LEFT_TRIM_HOVERED"
            when: leftTrimMa.containsMouse
            PropertyChanges { target: leftArrow; font.pixelSize: 40 }
            PropertyChanges { target: leftArrow; anchors.leftMargin: -10}
        },

        State {
            name: "RIGHT_TRIM_HOVERED"
            when: rightTrimMa.containsMouse
            PropertyChanges { target: rightArrow; font.pixelSize: 40 }
            PropertyChanges { target: rightArrow; anchors.rightMargin: -10}
        },

        State {
            name: "LEFT_TIME_HOVERED"
            when: leftTimeMa.containsMouse
            PropertyChanges { target: leftClockIcon; font.pixelSize: 18 }
            PropertyChanges { target: leftClock; anchors.leftMargin: 2 }
        },

        State {
            name: "RIGHT_TIME_HOVERED"
            when: rightTimeMa.containsMouse
            PropertyChanges { target: rightClockIcon; font.pixelSize: 18 }
            PropertyChanges { target: rightClock; anchors.rightMargin: 2 }
        }
    ]

    transitions: [
        Transition {
            from: "*"
            to: "*"
            reversible: true

            PropertyAnimation {
                target: leftArrow
                properties: "font.pixelSize"
                duration: animationDuration
                easing.type: Easing.Linear
            }
            PropertyAnimation {
                target: leftArrow
                properties: "anchors.leftMargin"
                duration: animationDuration
                easing.type: Easing.Linear
            }
            PropertyAnimation {
                target: rightArrow
                properties: "font.pixelSize"
                duration: animationDuration
                easing.type: Easing.Linear
            }
            PropertyAnimation {
                target: rightArrow
                properties: "anchors.rightMargin"
                duration: animationDuration
                easing.type: Easing.Linear
            }
            PropertyAnimation {
                target: leftClockIcon
                properties: "font.pixelSize"
                duration: animationDuration
                easing.type: Easing.Linear
            }
            PropertyAnimation {
                target: leftClock
                properties: "anchors.leftMargin"
                duration: animationDuration
                easing.type: Easing.Linear
            }
            PropertyAnimation {
                target: rightClockIcon
                properties: "font.pixelSize"
                duration: animationDuration
                easing.type: Easing.Linear
            }
            PropertyAnimation {
                target: rightClock
                properties: "anchors.rightMargin"
                duration: animationDuration
                easing.type: Easing.Linear
            }
        }
    ]
}
