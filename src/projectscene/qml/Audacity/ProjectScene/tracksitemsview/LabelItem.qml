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
    property var labelKey: null

    property alias navigation: navCtrl

    signal requestSelected()
    signal requestSelectionReset()

    signal titleEditAccepted(var newTitle)
    signal titleEditStarted()
    signal titleEditCanceled()

    signal labelItemMousePositionChanged(real x, real y)

    signal labelStartEditRequested()
    signal labelEndEditRequested()

    signal labelLeftStretchRequested(bool completed)
    signal labelRightStretchRequested(bool completed)

    property bool hover: root.containsMouse || root.headerHovered
    property bool headerHovered: false
    property bool containsMouse: false

    property color labelColor: null

    QtObject {
        id: prv

        property bool isPoint: root.width === 0

        readonly property int labelHeight: 14
        readonly property int earWidth: 7

        property color backgroundColor: root.labelColor
        property color leftEarBackgroundColor: root.labelColor
        property color rightEarBackgroundColor: root.labelColor
    }

    function setContainsMouse(containsMouse) {
        if (!root.enableCursorInteraction) {
            return
        }

        root.containsMouse = containsMouse
    }

    function editTitle() {
        header.edit()
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

    LabelContextMenuModel {
        id: labelContextMenuModel
        labelKey: root.labelKey

        onLabelEditRequested: {
            root.editTitle()
        }
    }

    ContextMenuLoader {
        id: labelContextMenuLoader

        onHandleMenuItem: function (itemId) {
            labelContextMenuModel.handleMenuItem(itemId)
        }
    }

    Component.onCompleted: {
        labelContextMenuModel.load()
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
            labelContextMenuLoader.show(Qt.point(e.x, e.y), labelContextMenuModel.items)
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
    LabelEar {
        id: leftEar

        height: prv.labelHeight
        x: -prv.earWidth - (prv.isPoint ? 0 : 1)

        isRight: false
        enableCursorInteraction: root.enableCursorInteraction
        backgroundColor: prv.leftEarBackgroundColor
        isSelected: root.isSelected

        onStretchStartRequested: {
            root.labelStartEditRequested()
        }

        onStretchMousePositionChanged: function(x, y) {
            root.labelItemMousePositionChanged(x, y)
        }

        onStretchRequested: function(completed) {
            root.labelLeftStretchRequested(completed)
        }

        onStretchEndRequested: {
            root.labelEndEditRequested()
        }
    }

    // Right Ear
    LabelEar {
        id: rightEar

        height: prv.labelHeight
        x: root.width + (prv.isPoint ? 0 : 1)

        isRight: true
        enableCursorInteraction: root.enableCursorInteraction
        backgroundColor: prv.rightEarBackgroundColor
        isSelected: root.isSelected

        onStretchStartRequested: {
            root.labelStartEditRequested()
        }

        onStretchMousePositionChanged: function(x, y) {
            root.labelItemMousePositionChanged(x, y)
        }

        onStretchRequested: function(completed) {
            root.labelRightStretchRequested(completed)
        }

        onStretchEndRequested: {
            root.labelEndEditRequested()
        }
    }

    // Main Label Header
    LabelHeader {
        id: header

        height: prv.labelHeight

        title: root.title

        isPoint: prv.isPoint

        backgroundColor: prv.backgroundColor

        earWidth: prv.earWidth

        enableCursorInteraction: root.enableCursorInteraction

        navigationPanel: root.labelNavigationPanel

        visible: root.visible

        onTitleEditAccepted: function(newTitle) {
            root.titleEditAccepted(newTitle)
        }

        onTitleEditStarted: {
            root.titleEditStarted()
        }

        onRequestSelected: {
            root.requestSelected()
        }

        onContextMenuOpenRequested: function(x, y) {
            labelContextMenuLoader.show(Qt.point(x, y), labelContextMenuModel.items)
        }

        onMousePositionChanged: function(x, y) {
            root.labelItemMousePositionChanged(x, y)
        }

        onHeaderHoveredChanged: function(value) {
            root.headerHovered = value
        }
    }

    // Point's Stalk
    LabelStalk {
        isForPoint: true
        enableCursorInteraction: root.enableCursorInteraction
        backgroundColor: leftEar.hovered ? prv.leftEarBackgroundColor : prv.rightEarBackgroundColor

        visible: prv.isPoint

        onHeaderHoveredChanged: function(value) {
            root.headerHovered = value
        }

        onMousePositionChanged: function(x, y) {
            root.labelItemMousePositionChanged(x, y)
        }

        onRequestSelected: {
            root.requestSelected()
        }
    }

    // Left Stalk
    LabelStalk {
        isRight: false
        enableCursorInteraction: root.enableCursorInteraction
        backgroundColor: prv.leftEarBackgroundColor
        isSelected: root.isSelected

        visible: !prv.isPoint

        onHeaderHoveredChanged: function(value) {
            root.headerHovered = value
        }

        onMousePositionChanged: function(x, y) {
            root.labelItemMousePositionChanged(x, y)
        }

        onRequestSelected: {
            root.requestSelected()
        }

        onStretchStartRequested: {
            root.labelStartEditRequested()
        }

        onStretchMousePositionChanged: function(x, y) {
            root.labelItemMousePositionChanged(x, y)
        }

        onStretchRequested: function(completed) {
            root.labelLeftStretchRequested(completed)
        }

        onStretchEndRequested: {
            root.labelEndEditRequested()
        }
    }

    // Right Stalk
    LabelStalk {
        isRight: true
        enableCursorInteraction: root.enableCursorInteraction
        backgroundColor: prv.rightEarBackgroundColor
        isSelected: root.isSelected

        visible: !prv.isPoint

        onHeaderHoveredChanged: function(value) {
            root.headerHovered = value
        }

        onMousePositionChanged: function(x, y) {
            root.labelItemMousePositionChanged(x, y)
        }

        onRequestSelected: {
            root.requestSelected()
        }

        onStretchStartRequested: {
            root.labelStartEditRequested()
        }

        onStretchMousePositionChanged: function(x, y) {
            root.labelItemMousePositionChanged(x, y)
        }

        onStretchRequested: function(completed) {
            root.labelRightStretchRequested(completed)
        }

        onStretchEndRequested: {
            root.labelEndEditRequested()
        }
    }

    states: [
        State {
            name: "NORMAL"
            when: !root.isSelected && !root.headerHovered && !leftEar.hovered && !rightEar.hovered
            PropertyChanges {
                target: prv
                backgroundColor: root.labelColor
                leftEarBackgroundColor: prv.backgroundColor
                rightEarBackgroundColor: prv.backgroundColor
            }
        },
        State {
            name: "SELECTED"
            when: root.isSelected && !root.headerHovered
            PropertyChanges {
                target: prv
                backgroundColor: ui.blendColors("#ffffff", root.labelColor, 0.4)
                leftEarBackgroundColor: prv.backgroundColor
                rightEarBackgroundColor: prv.backgroundColor
            }
        },
        State {
            name: "NORMAL_HOVERED"
            when: !root.isSelected && root.headerHovered
            PropertyChanges {
                target: prv
                backgroundColor: ui.blendColors("#ffffff", root.labelColor, 0.7)
                leftEarBackgroundColor: prv.backgroundColor
                rightEarBackgroundColor: prv.backgroundColor
            }
        },
        State {
            name: "LEFT_EAR_HOVERED"
            when: !root.isSelected && leftEar.hovered && !root.headerHovered
            PropertyChanges {
                target: prv
                backgroundColor: ui.blendColors("#ffffff", root.labelColor, 0.7)
                leftEarBackgroundColor: ui.blendColors("#ffffff", root.labelColor, 0.3)
                rightEarBackgroundColor: prv.backgroundColor
            }
        },
        State {
            name: "RIGHT_EAR_HOVERED"
            when: !root.isSelected && rightEar.hovered && !root.headerHovered
            PropertyChanges {
                target: prv
                backgroundColor: ui.blendColors("#ffffff", root.labelColor, 0.7)
                leftEarBackgroundColor: prv.backgroundColor
                rightEarBackgroundColor: ui.blendColors("#ffffff", root.labelColor, 0.3)
            }
        },
        State {
            name: "SELECTED_HOVERED"
            when: root.isSelected && root.headerHovered
            PropertyChanges {
                target: prv
                backgroundColor: ui.blendColors("#ffffff", root.labelColor, 0.2)
                leftEarBackgroundColor: prv.backgroundColor
                rightEarBackgroundColor: prv.backgroundColor
            }
        }
    ]
}
