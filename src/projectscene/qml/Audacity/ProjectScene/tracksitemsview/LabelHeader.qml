import QtQuick

import Muse.Ui
import Muse.UiComponents

Rectangle {
    id: root

    property string title: ""

    property bool isPoint: false

    property color backgroundColor: "transparent"
    property color labelColor: "#000000"

    property int earWidth: 0

    property bool enableCursorInteraction: true

    property var navigationPanel: null

    signal titleEditAccepted(var newTitle)
    signal titleEditStarted
    signal requestSelected

    signal contextMenuOpenRequested(real x, real y)
    signal mousePositionChanged(real x, real y)
    signal headerHoveredChanged(bool value)

    width: root.isPoint ? Math.max(50, titleLoader.contentWidth + 8) : parent.width
    x: root.isPoint ? root.earWidth + 3 : 0
    y: 0

    color: root.backgroundColor
    radius: root.isPoint ? 2 : 0

    function edit() {
        titleLoader.edit()
    }

    QtObject {
        id: prv

        readonly property int doubleClickInterval: 400
        readonly property int doubleClickMaxDistance: 5
    }

    MouseArea {
        id: headerDragArea
        anchors.fill: parent

        property var lastClickTime: 0
        property point doubleClickStartPosition

        acceptedButtons: Qt.LeftButton | Qt.RightButton
        hoverEnabled: true
        cursorShape: Qt.OpenHandCursor

        visible: root.enableCursorInteraction

        onPressed: function (e) {
            var currentTime = Date.now()

            if (e.button === Qt.RightButton) {
                return
            }

            if (currentTime - lastClickTime < prv.doubleClickInterval) {
                // Double click - edit title
                titleLoader.edit()
            } else {
                // Single click - select
                root.requestSelected()
                lastClickTime = currentTime
                doubleClickStartPosition = Qt.point(e.x, e.y)
            }
            e.accepted = false
        }

        onClicked: function (e) {
            if (e.button === Qt.RightButton) {
                root.contextMenuOpenRequested(e.x, e.y)
            }
            e.accepted = false
        }

        onPositionChanged: function (e) {
            // Reset double click timer if the mouse has moved,
            // to prevent rapid clip movement activate title editing
            if (Math.abs(e.x - doubleClickStartPosition.x) > prv.doubleClickMaxDistance || Math.abs(e.y - doubleClickStartPosition.y) > prv.doubleClickMaxDistance) {
                lastClickTime = 0
            }

            root.mousePositionChanged(e.x, e.y)

            e.accepted = false
        }

        onContainsMouseChanged: {
            if (!root.visible) {
                return
            }
            root.headerHoveredChanged(containsMouse)
        }
    }

    Loader {
        id: titleLoader

        property bool isEditState: false
        property real contentWidth: item ? (isEditState ? item.contentWidth : item.implicitWidth) : 0

        anchors.top: parent.top
        anchors.topMargin: isEditState ? 0 : 2
        anchors.left: parent.left
        anchors.leftMargin: root.isPoint ? 4 : root.earWidth
        anchors.right: parent.right
        anchors.rightMargin: root.isPoint ? 4 : root.earWidth
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 2

        sourceComponent: isEditState ? titleEditComp : titleComp

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
                color: root.labelColor
            }
        }

        Component {
            id: titleEditComp

            TextInputField {
                id: titleEdit

                property string newTitle: ""
                property real contentWidth: textMetrics.advanceWidth

                TextMetrics {
                    id: textMetrics
                    font: titleEdit.inputField.font
                    text: titleEdit.newTitle
                }

                background.color: root.backgroundColor
                background.border.width: 0
                background.radius: 0

                inputField.color: root.labelColor
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
            enabled: root.visible
            panel: root.navigationPanel
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
