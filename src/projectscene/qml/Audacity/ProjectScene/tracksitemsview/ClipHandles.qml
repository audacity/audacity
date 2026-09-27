import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Item {
    id: root

    property var canvas: null
    property bool handlesHovered: false
    property bool handlesVisible: false
    property bool altPressed: false

    property bool collapsed: false
    property int clipHeight: 114
    property int headerHeight: 20
    readonly property int handleMinH: 22
    readonly property int handleMaxH: 32
    readonly property int handleHeight: Math.min(handleMaxH, Math.max(handleMinH, collapsed ? Math.round(clipHeight / 3) : Math.round((clipHeight - headerHeight) / 3)))

    property int animationDuration: 100

    property bool debugRectsVisible: false

    property NavigationPanel clipNavigationPanel: null
    property bool leftTrimActive: false
    property bool rightTrimActive: false
    property bool leftRepeatActive: false
    property bool rightRepeatActive: false
    property bool leftStretchActive: false
    property bool rightStretchActive: false

    // mouse position event is not propagated on overlapping mouse areas
    // so we are handling it manually
    signal clipHandlesMousePositionChanged(real x, real y)

    property var trimStartPos

    signal clipStartEditRequested
    signal clipEndEditRequested
    signal cancelClipDragEditRequested

    signal trimLeftRequested(bool completed, int action)
    signal trimRightRequested(bool completed, int action)

    signal repeatLeftRequested(bool completed, int action)
    signal repeatRightRequested(bool completed, int action)

    signal stretchLeftRequested(bool completed, int action)
    signal stretchRightRequested(bool completed, int action)

    //! NOTE: auto-scroll for trimming is triggered from trackclipslistmodel
    signal stopAutoScroll

    //! Live preview of the tiles a repeat drag will create. Purely visual:
    //! the actual pasting happens once, when the drag is released.
    property int repeatGhostCount: 0
    property bool repeatGhostLeft: false
    property color clipColor: ui.theme.extra["clip_color_1"]
    property string clipTitle: ""

    Repeater {
        id: repeatGhosts

        model: root.repeatGhostCount

        // Mimics a real clip: clip-colored body with a header strip on top,
        // shown translucent as it is only a preview.
        Rectangle {
            id: ghostTile

            required property int index

            x: root.repeatGhostLeft ? -(index + 1) * root.width : (index + 1) * root.width
            y: root.collapsed ? 0 : -(root.headerHeight + 1)
            width: root.width
            height: root.clipHeight

            radius: 4
            color: root.clipColor
            opacity: 0.6
            border.width: 1
            border.color: ui.theme.extra["black_color"]

            Rectangle {
                anchors.top: parent.top
                anchors.left: parent.left
                anchors.right: parent.right
                height: root.collapsed ? 0 : root.headerHeight

                radius: ghostTile.radius
                color: ui.blendColors(ui.theme.extra["white_color"], root.clipColor, 0.3)

                // square off the header's bottom corners
                Rectangle {
                    anchors.bottom: parent.bottom
                    anchors.left: parent.left
                    anchors.right: parent.right
                    height: parent.radius
                    color: parent.color
                }

                StyledTextLabel {
                    anchors.fill: parent
                    anchors.leftMargin: 4
                    anchors.rightMargin: 8

                    text: root.clipTitle
                    horizontalAlignment: Qt.AlignLeft
                    opacity: 0.7
                }
            }
        }
    }

    Item {
        id: leftRepeatHandle

        x: -24
        y: 0
        height: root.handleHeight
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
            id: leftRepeat

            width: 14
            height: 14
            radius: 7

            anchors.verticalCenter: leftRepeatHandle.verticalCenter
            anchors.left: leftRepeatHandle.left
            anchors.leftMargin: 4

            color: ui.theme.extra["black_color"]
            border.width: 1
            border.color: ui.theme.extra["white_color"]

            StyledIconLabel {
                id: leftRepeatIcon
                width: 8
                anchors.verticalCenter: leftRepeat.verticalCenter
                anchors.horizontalCenter: leftRepeat.horizontalCenter

                iconCode: IconCode.LOOP
                font.pixelSize: 9
                color: ui.theme.extra["white_color"]
            }
        }

        MouseArea {
            id: leftRepeatMa

            anchors.fill: parent

            hoverEnabled: true

            cursorShape: Qt.BlankCursor

            Component.onCompleted: {
                CustomCursorProvider.setCursorShape(leftRepeatMa, ":/images/customCursorShapes/ClipRepeatLeft.png")
            }

            onPressed: {
                CustomCursorProvider.overrideCursor(":/images/customCursorShapes/ClipRepeatLeft.png")
                root.clipStartEditRequested()
            }

            onReleased: {
                CustomCursorProvider.restoreCursor()
                root.repeatGhostCount = 0
                root.repeatLeftRequested(true, ClipBoundaryAction.Auto)
                root.stopAutoScroll()
                root.clipEndEditRequested()
            }

            onEntered: {
                handlesHovered = true
            }

            onExited: {
                if (!leftTrimMa.containsMouse) {
                    handlesHovered = false
                }
            }

            onPositionChanged: {
                clipHandlesMousePositionChanged(mouseX + leftRepeatHandle.x, mouseY)
                if (pressed) {
                    root.repeatGhostLeft = true
                    root.repeatGhostCount = Math.max(0, Math.min(100, Math.floor(-(mouseX + leftRepeatHandle.x) / root.width)))
                    root.repeatLeftRequested(false, ClipBoundaryAction.Auto)
                }
            }

            onCanceled: {
                CustomCursorProvider.restoreCursor()
                root.repeatGhostCount = 0
                root.cancelClipDragEditRequested()
            }
        }

        // this must be on top of mouse areas to receive mouse events first
        NavigationControl {
            id: leftRepeatNavCtrl

            name: "LeftRepeatNavCtrl"
            enabled: root.handlesVisible

            panel: root.clipNavigationPanel
            column: 3

            onTriggered: {
                root.leftRepeatActive = !root.leftRepeatActive
            }

            onNavigationEvent: function(event) {
                if (!root.leftRepeatActive) {
                    return
                }

                switch (event.type) {
                case NavigationEvent.Left:
                    root.repeatLeftRequested(true, ClipBoundaryAction.Expand)
                    event.accepted = true
                    break
                case NavigationEvent.Right:
                    root.repeatLeftRequested(true, ClipBoundaryAction.Shrink)
                    event.accepted = true
                    break
                case NavigationEvent.Escape:
                    root.repeatLeftRequested(true, ClipBoundaryAction.Auto)
                    root.leftRepeatActive = false
                    event.accepted = true
                    break
                default:
                    break
                }
            }
        }

        NavigationFocusBorder {
            navigationCtrl: leftRepeatNavCtrl

            anchors.margins: 2

            drawOutsideParent: false
        }
    }

    Item {
        id: leftTrimHandle

        x: -24
        y: root.handleHeight
        height: root.handleHeight
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
            anchors.leftMargin: 2

            iconCode: IconCode.TRIM_HANDLE_LEFT
            font.pixelSize: 17
            color: ui.theme.extra["black_color"]
            style: Text.Outline
            styleColor: ui.theme.extra["white_color"]

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

                onNavigationEvent: function (event) {
                    if (!root.leftTrimActive) {
                        return
                    }

                    switch (event.type) {
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
            cursorShape: Qt.BlankCursor

            function updateCustomCursor() {
                var src = root.altPressed ? ":/images/customCursorShapes/ClipStretchLeft.png" : ":/images/customCursorShapes/ClipTrimLeft.png"
                CustomCursorProvider.setCursorShape(leftTrimMa, src)
            }

            Component.onCompleted: updateCustomCursor()

            Connections {
                target: root
                function onAltPressedChanged() {
                    leftTrimMa.updateCustomCursor()
                }
            }

            onPressed: function (e) {
                root.clipStartEditRequested()
            }

            onReleased: function (e) {
                root.trimLeftRequested(true, ClipBoundaryAction.Auto)
                root.stopAutoScroll();

                // this needs to be always at the very end
                root.clipEndEditRequested()
            }

            onEntered: {
                handlesHovered = true
            }

            onExited: {
                if (!leftTimeMa.containsMouse && !leftRepeatMa.containsMouse) {
                    handlesHovered = false
                }
            }

            onPositionChanged: function (e) {
                clipHandlesMousePositionChanged(mouseX + leftTrimHandle.x, mouseY)

                if (pressed) {
                    root.trimLeftRequested(false, ClipBoundaryAction.Auto)
                }
            }

            onCanceled: function (e) {
                cancelClipDragEditRequested()
            }
        }
    }

    Item {
        id: rightRepeatHandle

        x: parent.width - 12
        y: 0
        height: root.handleHeight
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
            id: rightRepeat

            width: 14
            height: 14
            radius: 7

            anchors.verticalCenter: rightRepeatHandle.verticalCenter
            anchors.right: rightRepeatHandle.right
            anchors.rightMargin: 4

            color: ui.theme.extra["black_color"]
            border.width: 1
            border.color: ui.theme.extra["white_color"]

            StyledIconLabel {
                id: rightRepeatIcon
                width: 8
                anchors.verticalCenter: rightRepeat.verticalCenter
                anchors.horizontalCenter: rightRepeat.horizontalCenter

                iconCode: IconCode.LOOP
                font.pixelSize: 9
                color: ui.theme.extra["white_color"]
            }
        }

        MouseArea {
            id: rightRepeatMa

            anchors.fill: parent

            hoverEnabled: true

            cursorShape: Qt.BlankCursor

            Component.onCompleted: {
                CustomCursorProvider.setCursorShape(rightRepeatMa, ":/images/customCursorShapes/ClipRepeatRight.png")
            }

            onPressed: function (e) {
                CustomCursorProvider.overrideCursor(":/images/customCursorShapes/ClipRepeatRight.png")
                root.clipStartEditRequested()
            }

            onReleased: function (e) {
                CustomCursorProvider.restoreCursor()
                root.repeatGhostCount = 0
                root.repeatRightRequested(true, ClipBoundaryAction.Auto)
                root.stopAutoScroll();

                // this needs to be always at the very end
                root.clipEndEditRequested()
            }

            onEntered: {
                handlesHovered = true
            }

            onExited: {
                if (!rightTrimMa.containsMouse) {
                    handlesHovered = false
                }
            }

            onPositionChanged: function (e) {
                clipHandlesMousePositionChanged(mouseX + rightRepeatHandle.x, mouseY)

                if (pressed) {
                    root.repeatGhostLeft = false
                    root.repeatGhostCount = Math.max(0, Math.min(100, Math.floor((mouseX + rightRepeatHandle.x - root.width) / root.width)))
                    root.repeatRightRequested(false, ClipBoundaryAction.Auto)
                }
            }

            onCanceled: function (e) {
                CustomCursorProvider.restoreCursor()
                root.repeatGhostCount = 0
                cancelClipDragEditRequested()
            }
        }

        // this must be on top of mouse areas to receive mouse events first
        NavigationControl {
            id: rightRepeatNavCtrl

            name: "RightRepeatNavCtrl"
            enabled: root.handlesVisible

            panel: root.clipNavigationPanel
            column: 7

            onTriggered: {
                root.rightRepeatActive = !root.rightRepeatActive
            }

            onNavigationEvent: function(event) {
                if (!root.rightRepeatActive) {
                    return
                }

                switch (event.type) {
                case NavigationEvent.Left:
                    root.repeatRightRequested(true, ClipBoundaryAction.Shrink)
                    event.accepted = true
                    break
                case NavigationEvent.Right:
                    root.repeatRightRequested(true, ClipBoundaryAction.Expand)
                    event.accepted = true
                    break
                case NavigationEvent.Escape:
                    root.repeatRightRequested(true, ClipBoundaryAction.Auto)
                    root.rightRepeatActive = false
                    event.accepted = true
                    break
                default:
                    break
                }
            }
        }

        NavigationFocusBorder {
            navigationCtrl: rightRepeatNavCtrl

            anchors.margins: 2

            drawOutsideParent: false
        }
    }

    Item {
        id: rightTrimHandle

        x: parent.width - 12
        y: root.handleHeight
        height: root.handleHeight
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
            anchors.rightMargin: 2

            iconCode: IconCode.TRIM_HANDLE_RIGHT
            font.pixelSize: 17
            color: ui.theme.extra["black_color"]
            style: Text.Outline
            styleColor: ui.theme.extra["white_color"]

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

                onNavigationEvent: function (event) {
                    if (!root.rightTrimActive) {
                        return
                    }

                    switch (event.type) {
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
            cursorShape: Qt.BlankCursor

            function updateCustomCursor() {
                var src = root.altPressed ? ":/images/customCursorShapes/ClipStretchRight.png" : ":/images/customCursorShapes/ClipTrimRight.png"
                CustomCursorProvider.setCursorShape(rightTrimMa, src)
            }

            Component.onCompleted: updateCustomCursor()

            Connections {
                target: root
                function onAltPressedChanged() {
                    rightTrimMa.updateCustomCursor()
                }
            }

            onPressed: function (e) {
                root.clipStartEditRequested()
            }

            onReleased: function (e) {
                root.trimRightRequested(true, ClipBoundaryAction.Auto)
                root.stopAutoScroll();

                // this needs to be always at the very end
                root.clipEndEditRequested()
            }

            onEntered: {
                handlesHovered = true
            }

            onExited: {
                if (!rightTimeMa.containsMouse && !rightRepeatMa.containsMouse) {
                    handlesHovered = false
                }
            }

            onPositionChanged: function (e) {
                clipHandlesMousePositionChanged(mouseX + rightTrimHandle.x, mouseY)

                if (pressed) {
                    root.trimRightRequested(false, ClipBoundaryAction.Auto)
                }
            }

            onCanceled: function (e) {
                cancelClipDragEditRequested()
            }
        }
    }

    Item {
        id: leftTimecode

        x: -24
        y: root.handleHeight * 2
        height: root.handleHeight
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

            color: ui.theme.extra["black_color"]

            StyledIconLabel {
                id: leftClockIcon

                anchors.centerIn: parent

                iconCode: IconCode.CLOCK
                font.pixelSize: 14
                color: ui.theme.extra["white_color"]

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

                onNavigationEvent: function (event) {
                    if (!root.leftStretchActive) {
                        return
                    }

                    switch (event.type) {
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
            cursorShape: Qt.BlankCursor

            Component.onCompleted: CustomCursorProvider.setCursorShape(leftTimeMa, ":/images/customCursorShapes/ClipStretchLeft.png")

            onPressed: {
                root.clipStartEditRequested()
            }

            onReleased: {
                root.stretchLeftRequested(true, ClipBoundaryAction.Auto)
                root.stopAutoScroll();

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

            onCanceled: function (e) {
                cancelClipDragEditRequested()
            }
        }
    }

    Item {
        id: rightTimecode

        x: parent.width - 12
        y: root.handleHeight * 2
        height: root.handleHeight
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

            color: ui.theme.extra["black_color"]

            StyledIconLabel {
                id: rightClockIcon

                anchors.centerIn: parent
                iconCode: IconCode.CLOCK
                font.pixelSize: 14
                color: ui.theme.extra["white_color"]

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

                    onNavigationEvent: function (event) {
                        if (!root.rightStretchActive) {
                            return
                        }

                        switch (event.type) {
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
            cursorShape: Qt.BlankCursor

            Component.onCompleted: CustomCursorProvider.setCursorShape(rightTimeMa, ":/images/customCursorShapes/ClipStretchRight.png")

            onPressed: {
                root.clipStartEditRequested()
            }

            onReleased: {
                root.stretchRightRequested(true, ClipBoundaryAction.Auto)
                root.stopAutoScroll();

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

            onCanceled: function (e) {
                cancelClipDragEditRequested()
            }
        }
    }

    state: "NORMAL"
    states: [
        State {
            name: "NORMAL"
            when: !leftTrimMa.containsMouse && !rightTrimMa.containsMouse && !leftTimeMa.containsMouse && !rightTimeMa.containsMouse && !leftRepeatMa.containsMouse && !rightRepeatMa.containsMouse
        },
        State {
            name: "LEFT_REPEAT_HOVERED"
            when: leftRepeatMa.containsMouse
            PropertyChanges {
                target: leftRepeat
                scale: 1.2
            }
        },
        State {
            name: "RIGHT_REPEAT_HOVERED"
            when: rightRepeatMa.containsMouse
            PropertyChanges {
                target: rightRepeat
                scale: 1.2
            }
        },
        State {
            name: "LEFT_TRIM_HOVERED"
            when: leftTrimMa.containsMouse
            PropertyChanges {
                target: leftArrow
                font.pixelSize: 22
            }
            PropertyChanges {
                target: leftArrow
                anchors.leftMargin: -1
            }
        },
        State {
            name: "RIGHT_TRIM_HOVERED"
            when: rightTrimMa.containsMouse
            PropertyChanges {
                target: rightArrow
                font.pixelSize: 22
            }
            PropertyChanges {
                target: rightArrow
                anchors.rightMargin: -1
            }
        },
        State {
            name: "LEFT_TIME_HOVERED"
            when: leftTimeMa.containsMouse
            PropertyChanges {
                target: leftClockIcon
                font.pixelSize: 18
            }
            PropertyChanges {
                target: leftClock
                anchors.leftMargin: 2
            }
        },
        State {
            name: "RIGHT_TIME_HOVERED"
            when: rightTimeMa.containsMouse
            PropertyChanges {
                target: rightClockIcon
                font.pixelSize: 18
            }
            PropertyChanges {
                target: rightClock
                anchors.rightMargin: 2
            }
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
            PropertyAnimation {
                target: leftRepeat
                properties: "scale"
                duration: animationDuration
                easing.type: Easing.Linear
            }
            PropertyAnimation {
                target: rightRepeat
                properties: "scale"
                duration: animationDuration
                easing.type: Easing.Linear
            }
        }
    ]
}
