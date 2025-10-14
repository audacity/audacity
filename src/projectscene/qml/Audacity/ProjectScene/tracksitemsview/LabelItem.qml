import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Item {
    id: root

    property string title: ""
    property bool isSelected: false
    property bool selectionInProgress: false
    property bool enableCursorInteraction: !selectionInProgress

    property alias navigation: navCtrl

    signal requestSelected
    signal requestSelectionReset

    signal titleEditAccepted(var newTitle)
    signal titleEditStarted
    signal titleEditCanceled

    signal labelItemMousePositionChanged(real x, real y)
    signal labelHeaderHoveredChanged(bool value)

    property bool hover: root.containsMouse || root.headerHovered
    property bool headerHovered: false
    property bool containsMouse: false

    onHeaderHoveredChanged: {
        labelHeaderHoveredChanged(headerHovered)
    }

    property color labelColor: null

    readonly property int labelHeight: 14
    readonly property int earWidth: 7
    readonly property int textTopPadding: 2
    readonly property int textBottomPadding: 2

    QtObject {
        id: prv

        readonly property int doubleClickInterval: 400
        readonly property int doubleClickMaxDistance: 5

        property real backgroundOpacity: 1
    }

    function setContainsMouse(containsMouse) {
        if (!root.enableCursorInteraction) {
            return
        }

        root.containsMouse = containsMouse
    }

    // Navigation support
    NavigationControl {
        id: navCtrl
        name: root.objectName || "LabelItem"
        enabled: root.enabled && root.visible

        accessible.role: MUAccessible.Button
        accessible.name: root.title

        onActiveChanged: function (active) {
            if (active) {
                root.forceActiveFocus()
            }
        }

        onTriggered: {
            root.requestSelected()
        }
    }

    NavigationFocusBorder {
        navigationCtrl: navCtrl
    }

    // panel for navigating within the label's items
    property NavigationPanel labelNavigationPanel: NavigationPanel {
        name: "ClipNavigationPanel"
        enabled: navCtrl.active
        direction: NavigationPanel.Horizontal
        section: navigation.panel.section
        onActiveChanged: function (active) {
            if (active) {
                root.forceActiveFocus()
            }
        }

        onNavigationEvent: function (event) {
            if (event.type === NavigationEvent.Escape) {
                navCtrl.requestActive()
            }
        }
    }

    MouseArea {
        id: hoverArea
        anchors.fill: parent

        hoverEnabled: true
        cursorShape: Qt.IBeamCursor
        acceptedButtons: Qt.RightButton

        visible: root.enableCursorInteraction

        onVisibleChanged: {
            root.setContainsMouse(containsMouse)
        }

        onClicked: function (e) {
            root.requestSelected()
        }

        onPositionChanged: function (e) {
            labelItemMousePositionChanged(e.x, e.y);
        }

        onContainsMouseChanged: {
            if (!root.visible) {
                return
            }

            root.setContainsMouse(containsMouse)
        }
    }

    // Left Ear
    Canvas {
        id: leftEar
        width: root.earWidth
        height: root.labelHeight
        x: -root.earWidth - 1
        opacity: prv.backgroundOpacity

        onPaint: {
            var ctx = getContext("2d")
            ctx.clearRect(0, 0, width, height)

            ctx.fillStyle = root.labelColor
            ctx.strokeStyle = root.labelColor
            ctx.lineWidth = 1
            ctx.imageSmoothingEnabled = true

            ctx.beginPath()

            const radius = 1
            ctx.moveTo(0, 1)
            ctx.arcTo(0, 0, radius, 0, radius)
            ctx.lineTo(width, 0)
            ctx.lineTo(width + 1, height)

            ctx.closePath()

            ctx.fill()
            ctx.stroke()
        }

        Connections {
            target: root
            function onLabelColorChanged() {
                leftEar.requestPaint()
            }
        }

        MouseArea {
            id: leftEarDragArea
            anchors.fill: parent

            acceptedButtons: Qt.LeftButton
            hoverEnabled: true
            cursorShape: Qt.SizeHorCursor

            visible: root.enableCursorInteraction

            onContainsMouseChanged: {
                if (!root.visible) {
                    return
                }
                root.headerHovered = containsMouse
            }
        }
    }

    // Right Ear
    Canvas {
        id: rightEar
        width: root.earWidth
        height: root.labelHeight
        x: root.width + 1
        opacity: prv.backgroundOpacity

        onPaint: {
            var ctx = getContext("2d")
            ctx.clearRect(0, 0, width, height)

            ctx.fillStyle = root.labelColor
            ctx.strokeStyle = root.labelColor
            ctx.lineWidth = 1
            ctx.imageSmoothingEnabled = true

            ctx.beginPath()

            ctx.moveTo(0, 0)
            ctx.lineTo(0, height)
            ctx.lineTo(width, 0)

            ctx.closePath()

            ctx.fill()
            ctx.stroke()
        }

        Connections {
            target: root
            function onLabelColorChanged() {
                rightEar.requestPaint()
            }
        }

        MouseArea {
            id: rightEarDragArea
            anchors.fill: parent

            acceptedButtons: Qt.LeftButton
            hoverEnabled: true
            cursorShape: Qt.SizeHorCursor

            visible: root.enableCursorInteraction

            onContainsMouseChanged: {
                if (!root.visible) {
                    return
                }
                root.headerHovered = containsMouse
            }
        }
    }

    // Main Label Header
    Rectangle {
        id: header
        width: parent.width
        height: root.labelHeight
        color: root.labelColor
        opacity: prv.backgroundOpacity

        MouseArea {
            id: headerDragArea
            anchors.fill: parent

            property var lastClickTime: 0
            property point doubleClickStartPosition

            acceptedButtons: Qt.LeftButton
            hoverEnabled: true
            cursorShape: Qt.OpenHandCursor

            visible: root.enableCursorInteraction

            onContainsMouseChanged: {
                if (!root.visible) {
                    return
                }
                root.headerHovered = containsMouse
            }

            onPressed: function (e) {
                console.log("============= click")
                var currentTime = Date.now()

                if (currentTime - lastClickTime < prv.doubleClickInterval) {
                    // Double click - edit title
                    titleLoader.edit()
                } else {
                    // Single click - select
                    root.requestSelected()
                    lastClickTime = currentTime
                }
                e.accepted = false
            }

            onPositionChanged: function (e) {
                // Reset double click timer if the mouse has moved,
                // to prevent rapid clip movement activate title editing
                if (Math.abs(e.x - doubleClickStartPosition.x) > prv.doubleClickMaxDistance ||
                    Math.abs(e.y - doubleClickStartPosition.y) > prv.doubleClickMaxDistance) {

                    lastClickTime = 0
                }

                root.labelItemMousePositionChanged(e.x, e.y)

                e.accepted = false
            }
        }

        Loader {
            id: titleLoader

            anchors.top: parent.top
            anchors.topMargin: 2
            anchors.left: parent.left
            anchors.leftMargin: root.earWidth
            anchors.right: parent.right
            anchors.rightMargin: root.earWidth
            anchors.bottom: parent.bottom
            anchors.bottomMargin: 2

            property bool isEditState: false
            sourceComponent: isEditState ? titleEditComp : titleComp

            property color labelColor: "#000000"

            function edit() {
                root.titleEditStarted()

                titleLoader.isEditState = true
                titleLoader.item.currentText = root.title
                titleLoader.item.newTitle = root.title
                titleLoader.item.ensureActiveFocus()
            }

            Component {
                id: titleComp

                StyledTextLabel {
                    text: root.title
                    horizontalAlignment: Qt.AlignLeft
                    color: titleLoader.labelColor
                }
            }

            Component {
                id: titleEditComp

                TextInputField {
                    id: titleEdit

                    property string newTitle: ""

                    background.color: header.color
                    background.border.width: 0
                    background.radius: 0
                    inputField.color: titleLoader.labelColor
                    textSidePadding: 0

                    onTextChanged: function (text) {
                        titleEdit.newTitle = text
                    }

                    onAccepted: {
                        Qt.callLater(root.titleEditAccepted, newTitle)

                        titleLoader.isEditState = false
                    }

                    onEscaped: {
                        titleLoader.isEditState = false
                    }

                    onFocusChanged: {
                        if (!titleEdit.focus) {
                            titleEdit.visible = false
                            titleEdit.accepted()
                        }
                    }
                }
            }

            NavigationControl {
                id: titleEditNavCtrl
                name: "TitleEditNavCtrl"
                enabled: /*root.enabled && */root.visible
                panel: root.labelNavigationPanel
                column: 3

                accessible.enabled: titleEditNavCtrl.enabled

                onTriggered: {
                    titleLoader.edit()
                }
            }

            NavigationFocusBorder {
                navigationCtrl: titleEditNavCtrl

                anchors.topMargin: 1
                anchors.bottomMargin: 0
                radius: 5
            }
        }
    }

    // Left Stalk
    Rectangle {
        id: leftStalk

        width: 1
        height: parent.height
        x: -1

        color: root.labelColor
        opacity: prv.backgroundOpacity

        MouseArea {
            anchors.fill: parent
            anchors.leftMargin: -1
            anchors.rightMargin: -1

            acceptedButtons: Qt.LeftButton
            hoverEnabled: true
            cursorShape: pressed ? Qt.CloseHandCursor : Qt.OpenHandCursor

            visible: root.enableCursorInteraction

            onContainsMouseChanged: {
                if (!root.visible) {
                    return
                }
                root.headerHovered = containsMouse
            }
        }
    }

    // Right Stalk
    Rectangle {
        id: rightStalk

        width: 1
        height: parent.height
        x: parent.width

        color: root.labelColor
        opacity: prv.backgroundOpacity

        MouseArea {
            anchors.fill: parent
            anchors.leftMargin: -1
            anchors.rightMargin: -1

            acceptedButtons: Qt.LeftButton
            hoverEnabled: true
            cursorShape: pressed ? Qt.CloseHandCursor : Qt.OpenHandCursor

            visible: root.enableCursorInteraction

            onContainsMouseChanged: {
                if (!root.visible) {
                    return
                }
                root.headerHovered = containsMouse
            }
        }
    }

    states: [
        State {
            name: "NORMAL"
            when: !root.isSelected && !root.headerHovered
            PropertyChanges {
                target: prv
                backgroundOpacity: 1
            }
        },
        State {
            name: "SELECTED"
            when: root.isSelected && !root.headerHovered
            PropertyChanges {
                target: prv
                backgroundOpacity: 0.4
            }
        },
        State {
            name: "NORMAL_HOVERED"
            when: !root.isSelected && root.headerHovered
            PropertyChanges {
                target: prv
                backgroundOpacity: 0.7
            }
        }
        ,
        State {
            name: "SELECTED_HOVERED"
            when: root.isSelected && root.headerHovered
            PropertyChanges {
                target: prv
                backgroundOpacity: 0.4
            }
        }
    ]
}

