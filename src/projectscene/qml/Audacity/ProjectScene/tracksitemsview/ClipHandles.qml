import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Item {
    id: root

    property var canvas: null
    property bool handlesHovered: false
    property bool handlesVisible: false

    property int animationDuration: 100

    property bool debugRectsVisible: false

    property NavigationPanel clipNavigationPanel: null
    property bool leftTrimActive: false
    property bool rightTrimActive: false
    property bool leftStretchActive: false
    property bool rightStretchActive: false

    // mouse position event is not propagated on overlapping mouse areas
    // so we are handling it manually
    signal clipHandlesMousePositionChanged(real x, real y)

    property var trimStartPos

    signal clipStartEditRequested()
    signal clipEndEditRequested()

    signal trimLeftRequested(bool completed, int action)
    signal trimRightRequested(bool completed, int action)

    signal stretchLeftRequested(bool completed, int action)
    signal stretchRightRequested(bool completed, int action)

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

            NavigationControl {
                id: leftTrimNavCtrl
                name: "LeftArrowNavCtrl"
                enabled: handlesVisible
                panel: root.clipNavigationPanel
                column: 2

                accessible.enabled: leftTrimNavCtrl.enabled

                onTriggered: {
                    root.leftTrimActive = !root.leftTrimActive
                }

                onNavigationEvent: function(event) {
                    if (!root.leftTrimActive) {
                        return
                    }

                    switch(event.type) {
                    case NavigationEvent.Left:
                        root.trimLeftRequested(true, ClipBoundaryAction.Expand)
                        event.accepted = true
                        break
                    case NavigationEvent.Right:
                        root.trimLeftRequested(true, ClipBoundaryAction.Shrink)
                        event.accepted = true
                        break
                    case NavigationEvent.Trigger:
                        // NOTE: do not modify leftTrimActive, otherwise it breaks
                        // trim mode activation/deactivation
                        break
                    case NavigationEvent.Escape:
                        root.leftTrimActive = false
                        event.accepted = true
                        break
                    default:
                        root.leftTrimActive = false
                        break
                    }
                }

                onActiveChanged: {
                    if (!active) {
                        root.leftTrimActive = false
                    }
                }
            }

            NavigationFocusBorder {
                navigationCtrl: leftTrimNavCtrl

                radius: 5

                border.color: root.leftTrimActive ? "orange" : ui.theme.fontPrimaryColor
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
                root.trimLeftRequested(true, ClipBoundaryAction.Auto)
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
                    root.trimLeftRequested(false, ClipBoundaryAction.Auto)
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

            NavigationControl {
                id: rightTrimNavCtrl
                name: "RightArrowNavCtrl"
                enabled: handlesVisible
                panel: root.clipNavigationPanel
                column: 5

                accessible.enabled: rightTrimNavCtrl.enabled

                onTriggered: {
                    root.rightTrimActive = !root.rightTrimActive
                }

                onNavigationEvent: function(event) {
                    if (!root.rightTrimActive) {
                        return
                    }

                    switch(event.type) {
                    case NavigationEvent.Left:
                        root.trimRightRequested(true, ClipBoundaryAction.Shrink)
                        event.accepted = true
                        break
                    case NavigationEvent.Right:
                        root.trimRightRequested(true, ClipBoundaryAction.Expand)
                        event.accepted = true
                        break
                    case NavigationEvent.Trigger:
                        // NOTE: do not modify rightTrimActive, otherwise it breaks
                        // trim mode activation/deactivation
                        break
                    case NavigationEvent.Escape:
                        root.rightTrimActive = false
                        event.accepted = true
                        break
                    default:
                        root.rightTrimActive = false
                        break
                    }
                }

                onActiveChanged: {
                    if (!active) {
                        root.rightTrimActive = false
                    }
                }
            }

            NavigationFocusBorder {
                navigationCtrl: rightTrimNavCtrl

                radius: 5

                border.color: root.rightTrimActive ? "orange" : ui.theme.fontPrimaryColor
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
                root.trimRightRequested(true, ClipBoundaryAction.Auto)
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
                    root.trimRightRequested(false, ClipBoundaryAction.Auto)
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

            NavigationControl {
                id: leftStretchNavCtrl
                name: "LeftStretchNavCtrl"
                enabled: handlesVisible
                panel: root.clipNavigationPanel
                column: 1

                accessible.enabled: leftStretchNavCtrl.enabled

                onTriggered: {
                    root.leftStretchActive = !root.leftStretchActive
                }

                onNavigationEvent: function(event) {
                    if (!root.leftStretchActive) {
                        return
                    }

                    switch(event.type) {
                    case NavigationEvent.Left:
                        root.stretchLeftRequested(true, ClipBoundaryAction.Expand)
                        event.accepted = true
                        break
                    case NavigationEvent.Right:
                        root.stretchLeftRequested(true, ClipBoundaryAction.Shrink)
                        event.accepted = true
                        break
                    case NavigationEvent.Trigger:
                        // NOTE: do not modify leftStretchActive, otherwise it breaks
                        // stretch mode activation/deactivation
                        break
                    case NavigationEvent.Escape:
                        root.leftStretchActive = false
                        event.accepted = true
                        break
                    default:
                        root.leftStretchActive = false
                        break
                    }
                }

                onActiveChanged: {
                    if (!active) {
                        root.leftStretchActive = false
                    }
                }
            }

            Rectangle {
                x: -9
                y: -9
                height: 30
                width: 30
                color: "transparent"

                NavigationFocusBorder {
                    navigationCtrl: leftStretchNavCtrl

                    radius: 5

                    border.color: root.leftStretchActive ? "orange" : ui.theme.fontPrimaryColor
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
                root.stretchLeftRequested(true, ClipBoundaryAction.Auto)
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
                    root.stretchLeftRequested(false, ClipBoundaryAction.Auto)
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

                NavigationControl {
                    id: rightStretchNavCtrl
                    name: "RightStretchNavCtrl"
                    enabled: handlesVisible
                    panel: root.clipNavigationPanel
                    column: 6

                    accessible.enabled: rightStretchNavCtrl.enabled

                    onTriggered: {
                        root.rightStretchActive = !root.rightStretchActive
                    }

                    onNavigationEvent: function(event) {
                        if (!root.rightStretchActive) {
                            return
                        }

                        switch(event.type) {
                        case NavigationEvent.Left:
                            root.stretchRightRequested(true, ClipBoundaryAction.Shrink)
                            event.accepted = true
                            break
                        case NavigationEvent.Right:
                            root.stretchRightRequested(true, ClipBoundaryAction.Expand)
                            event.accepted = true
                            break
                        case NavigationEvent.Trigger:
                            // NOTE: do not modify rightStretchActive, otherwise it breaks
                            // stretch mode activation/deactivation
                            break
                        case NavigationEvent.Escape:
                            root.rightStretchActive = false
                            event.accepted = true
                            break
                        default:
                            root.rightStretchActive = false
                            break
                        }
                    }

                    onActiveChanged: {
                        if (!active) {
                            root.rightStretchActive = false
                        }
                    }
                }

                Rectangle {
                    x: -8
                    y: -8
                    height: 30
                    width: 30
                    color: "transparent"

                    NavigationFocusBorder {
                        navigationCtrl: rightStretchNavCtrl

                        radius: 5

                        border.color: root.rightStretchActive ? "orange" : ui.theme.fontPrimaryColor
                    }
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
                root.stretchRightRequested(true, ClipBoundaryAction.Auto)
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
                    root.stretchRightRequested(false, ClipBoundaryAction.Auto)
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
