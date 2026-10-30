import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Item {
    id: root

    property var canvas: null
    property bool handlesVisible: false
    property bool altPressed: false

    property bool collapsed: false
    property int clipHeight: 114
    property int headerHeight: 20
    readonly property int handleHeight: Math.min(prv.handleMaxH, Math.max(prv.handleMinH, collapsed ? Math.round(clipHeight / 2) : Math.round((clipHeight - headerHeight) / 2)))

    // Item that the trim handles should not steal input from
    property Item exclusionItem: null

    function isExcluded(mouseArea, point) {
        const item = root.exclusionItem
        if (!item || !item.visible) {
            return false
        }

        const mapped = mouseArea.mapToItem(item, point.x, point.y)
        return item.contains(mapped)
    }

    property NavigationPanel clipNavigationPanel: null
    property bool leftTrimActive: false
    property bool rightTrimActive: false
    property bool leftStretchActive: false
    property bool rightStretchActive: false

    // mouse position event is not propagated on overlapping mouse areas
    // so we are handling it manually
    signal clipHandlesMousePositionChanged(real x, real y)

    signal clipStartEditRequested
    signal clipEndEditRequested
    signal cancelClipDragEditRequested

    signal trimLeftRequested(bool completed, int action)
    signal trimRightRequested(bool completed, int action)

    signal stretchLeftRequested(bool completed, int action)
    signal stretchRightRequested(bool completed, int action)

    //! NOTE: auto-scroll for trimming is triggered from trackclipslistmodel
    signal stopAutoScroll

    QtObject {
        id: prv

        readonly property int handleMinH: 22
        readonly property int handleMaxH: 32
        readonly property int handleWidth: 30
        readonly property int handleClipOverlap: 6

        // icons are centred, so that they grow around their centre when hovered
        readonly property int trimIconMargin: 2
        readonly property real trimIconCenterOffset: trimIconMargin + leftArrow.iconSize / 2

        readonly property int clockMargin: 4
        readonly property int clockIconSize: 14
        readonly property int clockIconHoveredSize: 18
        readonly property real clockCenterOffset: clockMargin + clockCircleSize(clockIconSize) / 2

        function clockCircleSize(clockIconSize) {
            return clockIconSize - 2
        }

        readonly property bool debugRectsVisible: false
    }

    Item {
        id: leftTrimHandle

        x: prv.handleClipOverlap - prv.handleWidth
        height: root.handleHeight
        width: prv.handleWidth

        visible: handlesVisible

        Rectangle {
            anchors.fill: parent

            color: "transparent"
            border.width: 1
            border.color: "blue"

            visible: prv.debugRectsVisible
        }

        ClipHandleIcon {
            id: leftArrow
            anchors.verticalCenter: leftTrimHandle.verticalCenter
            anchors.horizontalCenter: leftTrimHandle.left
            anchors.horizontalCenterOffset: prv.trimIconCenterOffset

            hovered: leftTrimMa.containsMouse
            iconCode: IconCode.TRIM_HANDLE_LEFT
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

                visible: prv.debugRectsVisible
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
                root.stopAutoScroll()

                // this needs to be always at the very end
                root.clipEndEditRequested()
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
        id: rightTrimHandle

        x: parent.width - prv.handleClipOverlap
        height: root.handleHeight
        width: prv.handleWidth

        visible: handlesVisible

        Rectangle {
            anchors.fill: parent

            color: "transparent"
            border.width: 1
            border.color: "blue"

            visible: prv.debugRectsVisible
        }

        ClipHandleIcon {
            id: rightArrow
            anchors.verticalCenter: rightTrimHandle.verticalCenter
            anchors.horizontalCenter: rightTrimHandle.right
            anchors.horizontalCenterOffset: -prv.trimIconCenterOffset

            hovered: rightTrimMa.containsMouse
            iconCode: IconCode.TRIM_HANDLE_RIGHT
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

                visible: prv.debugRectsVisible
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

            containmentMask: QtObject {
                function contains(point: point): bool {
                    return point.x >= 0 && point.y >= 0 && point.x < rightTrimMa.width && point.y < rightTrimMa.height && !root.isExcluded(rightTrimMa, point)
                }
            }

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
                root.stopAutoScroll()

                // this needs to be always at the very end
                root.clipEndEditRequested()
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

        x: prv.handleClipOverlap - prv.handleWidth
        y: leftTrimHandle.height
        height: root.handleHeight
        width: prv.handleWidth

        visible: handlesVisible

        Rectangle {
            anchors.fill: parent

            color: "transparent"
            border.width: 1
            border.color: "blue"

            visible: prv.debugRectsVisible
        }

        Rectangle {
            id: leftClock

            width: prv.clockCircleSize(leftClockIcon.font.pixelSize)
            height: prv.clockCircleSize(leftClockIcon.font.pixelSize)
            radius: prv.clockCircleSize(leftClockIcon.font.pixelSize) / 2
            anchors.verticalCenter: leftTimecode.verticalCenter
            anchors.horizontalCenter: leftTimecode.left
            anchors.horizontalCenterOffset: prv.clockCenterOffset

            color: ui.theme.extra["black_color"]

            ClipHandleIcon {
                id: leftClockIcon

                anchors.centerIn: parent

                hovered: leftTimeMa.containsMouse
                iconSize: prv.clockIconSize
                hoveredIconSize: prv.clockIconHoveredSize
                iconCode: IconCode.CLOCK
                color: ui.theme.extra["white_color"]

                Rectangle {
                    height: 12
                    width: 12
                    anchors.verticalCenter: leftClockIcon.verticalCenter
                    anchors.horizontalCenter: leftClockIcon.horizontalCenter
                    color: "transparent"

                    border.color: "red"
                    border.width: 1

                    visible: prv.debugRectsVisible
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
                root.stopAutoScroll()

                // this needs to be always at the very end
                root.clipEndEditRequested()
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

        x: parent.width - prv.handleClipOverlap
        y: rightTrimHandle.height
        height: root.handleHeight
        width: prv.handleWidth

        visible: handlesVisible

        Rectangle {
            anchors.fill: parent

            color: "transparent"
            border.width: 1
            border.color: "blue"
            visible: prv.debugRectsVisible
        }

        Rectangle {
            id: rightClock

            width: prv.clockCircleSize(rightClockIcon.font.pixelSize)
            height: prv.clockCircleSize(rightClockIcon.font.pixelSize)
            radius: prv.clockCircleSize(rightClockIcon.font.pixelSize) / 2
            anchors.verticalCenter: rightTimecode.verticalCenter
            anchors.horizontalCenter: rightTimecode.right
            anchors.horizontalCenterOffset: -prv.clockCenterOffset

            color: ui.theme.extra["black_color"]

            ClipHandleIcon {
                id: rightClockIcon

                anchors.centerIn: parent
                hovered: rightTimeMa.containsMouse
                iconSize: prv.clockIconSize
                hoveredIconSize: prv.clockIconHoveredSize
                iconCode: IconCode.CLOCK
                color: ui.theme.extra["white_color"]

                Rectangle {
                    height: 12
                    width: 12
                    anchors.verticalCenter: rightClockIcon.verticalCenter
                    anchors.horizontalCenter: rightClockIcon.horizontalCenter
                    color: "transparent"

                    border.color: "red"
                    border.width: 1
                    visible: prv.debugRectsVisible
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
                root.stopAutoScroll()

                // this needs to be always at the very end
                root.clipEndEditRequested()
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
}
