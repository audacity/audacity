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

    property bool isLeftLinked: false
    property bool isRightLinked: false
    property bool isLinkedActive: false

    property int visualWidth: prv.isPoint ? pointStalk.width + header.x + header.width : header.width
    readonly property int headerDefaultHeight: 14

    property var container: null

    property bool hover: root.containsMouse || root.headerHovered
    property bool headerHovered: false
    property bool containsMouse: false

    property color labelColor: null

    property alias navigation: navCtrl

    height: header.height

    signal requestSelected()
    signal requestSingleSelected()
    signal requestSelectionReset()

    signal titleEditStarted()
    signal titleEditAccepted(var newTitle)
    signal titleEditCanceled()
    signal titleEditFinished()

    signal labelItemMousePositionChanged(real x, real y)

    signal labelStartEditRequested()
    signal labelEndEditRequested()

    signal labelLeftStretchRequested(bool unlink, bool completed)
    signal labelRightStretchRequested(bool unlink, bool completed)

    signal activateLeftLinkedLabel()
    signal activateRightLinkedLabel()
    signal deactivateLinkedLabel()

    QtObject {
        id: prv

        property bool isPoint: root.width < 2
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
            root.requestSingleSelected()
        }
    }

    NavigationFocusBorder {
        navigationCtrl: navCtrl
    }

    // panel for navigating within the label's items
    property NavigationPanel labelNavigationPanel: NavigationPanel {
        name: "LabelNavigationPanel"
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

    Component.onDestruction: {
        //! NOTE The outer component uses this information to handle the current cursor.
        // It is important to cleanup this state before removing the component
        // to prevent the cursor from being stuck in the wrong state.
        root.headerHovered = false
    }

    // Left Ear
    LabelEar {
        id: leftEar

        //! To draw on top of other labels, we need to change the parent and increase z
        parent: root.container
        x: root.parent.x - prv.earWidth - (prv.isPoint ? 0 : 1)
        y: root.parent.y
        z: root.parent.z

        height: root.headerDefaultHeight

        isRight: false
        enableCursorInteraction: root.enableCursorInteraction
        backgroundColor: prv.leftEarBackgroundColor
        isSelected: root.isSelected
        isLinked: root.isLeftLinked

        onHoveredChanged: {
            if (hovered && isLinked) {
                root.activateLeftLinkedLabel()
            } else if (!hovered && isLinked) {
                root.deactivateLinkedLabel()
            }
        }

        onStretchStartRequested: {
            root.requestSingleSelected()
            root.labelStartEditRequested()
        }

        onStretchMousePositionChanged: function(x, y) {
            root.labelItemMousePositionChanged(x - root.parent.x, y)
        }

        onStretchRequested: function(completed) {
            root.labelLeftStretchRequested(true /*unlink*/, completed)
        }

        onStretchEndRequested: {
            root.labelEndEditRequested()
        }
    }

    // Right Ear
    LabelEar {
        id: rightEar

        //! To draw on top of other labels, we need to change the parent and increase z
        parent: root.container
        x: root.parent.x + root.width + (prv.isPoint ? 0 : 1)
        y: root.parent.y
        z: root.parent.z

        height: root.headerDefaultHeight

        isRight: true
        enableCursorInteraction: root.enableCursorInteraction
        backgroundColor: prv.rightEarBackgroundColor
        isSelected: root.isSelected
        isLinked: root.isRightLinked

        onHoveredChanged: {
            if (hovered && isLinked) {
                root.activateRightLinkedLabel()
            } else if (!hovered && isLinked) {
                root.deactivateLinkedLabel()
            }
        }

        onStretchStartRequested: {
            root.requestSingleSelected()
            root.labelStartEditRequested()
        }

        onStretchMousePositionChanged: function(x, y) {
            root.labelItemMousePositionChanged(x - root.parent.x, y)
        }

        onStretchRequested: function(completed) {
            root.labelRightStretchRequested(true /*unlink*/, completed)
        }

        onStretchEndRequested: {
            root.labelEndEditRequested()
        }
    }

    // Main Label Header
    LabelHeader {
        id: header

        width: prv.isPoint ? contentWidth : root.width
        height: Math.min(root.height, contentHeight)

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

        onEditStarted: {
            root.titleEditStarted()
        }

        onEditFinished: {
            root.titleEditFinished()
        }

        onRequestSingleSelected: {
            root.requestSingleSelected()
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
        id: pointStalk

        //! To draw on top of other labels, we need to change the parent and increase z
        parent: root.container
        width: 2
        x: root.parent.x + root.parent.width/2 - width/2
        y: root.parent.y
        z: root.parent.z

        height: root.height

        isForPoint: true
        enableCursorInteraction: root.enableCursorInteraction
        backgroundColor: leftEar.hovered ? prv.leftEarBackgroundColor : prv.rightEarBackgroundColor

        visible: prv.isPoint

        onHeaderHoveredChanged: function(value) {
            root.headerHovered = value
        }

        onRequestSelected: {
            root.requestSingleSelected()
        }

        onStretchMousePositionChanged: function(x, y) {
            root.labelItemMousePositionChanged(x - root.parent.x, y)
        }

        onStretchRequested: function(completed) {
            root.labelLeftStretchRequested(false /*without unlink*/, completed)
        }

        onStretchEndRequested: {
            root.labelEndEditRequested()
        }
    }

    // Left Stalk
    LabelStalk {
        id: leftStalk

        property bool isLinked: root.isLeftLinked

        //! To draw on top of other labels, we need to change the parent and increase z
        parent: root.container
        x: root.parent.x - 1
        y: root.parent.y
        z: root.parent.z

        height: root.height

        isRight: false
        enableCursorInteraction: root.enableCursorInteraction
        backgroundColor: prv.leftEarBackgroundColor
        isSelected: root.isSelected

        visible: !prv.isPoint || isStretchInProgress

        onHeaderHoveredChanged: function(value) {
            root.headerHovered = value
        }

        onHoveredChanged: {
            if (hovered && isLinked) {
                root.activateLeftLinkedLabel()
            } else if (!hovered && isLinked) {
                root.deactivateLinkedLabel()
            }
        }

        onStretchStartRequested: {
            root.requestSingleSelected()
            root.labelStartEditRequested()
        }

        onStretchMousePositionChanged: function(x, y) {
            root.labelItemMousePositionChanged(x - root.parent.x, y)
        }

        onStretchRequested: function(completed) {
            root.labelLeftStretchRequested(false /*without unlink*/, completed)
        }

        onStretchEndRequested: {
            root.labelEndEditRequested()
        }
    }

    // Right Stalk
    LabelStalk {
        id: rightStalk

        property bool isLinked: root.isRightLinked

        //! To draw on top of other labels, we need to change the parent and increase z
        parent: root.container
        x: root.parent.x + root.width
        y: root.parent.y
        z: root.parent.z

        height: root.height

        isRight: true
        enableCursorInteraction: root.enableCursorInteraction
        backgroundColor: prv.rightEarBackgroundColor
        isSelected: root.isSelected

        visible: !prv.isPoint || isStretchInProgress

        onHeaderHoveredChanged: function(value) {
            root.headerHovered = value
        }

        onHoveredChanged: {
            if (hovered && isLinked) {
                root.activateRightLinkedLabel()
            } else if (!hovered && isLinked) {
                root.deactivateLinkedLabel()
            }
        }

        onStretchStartRequested: {
            root.requestSingleSelected()
            root.labelStartEditRequested()
        }

        onStretchMousePositionChanged: function(x, y) {
            root.labelItemMousePositionChanged(x - root.parent.x, y)
        }

        onStretchRequested: function(completed) {
            root.labelRightStretchRequested(false /*without unlink*/, completed)
        }

        onStretchEndRequested: {
            root.labelEndEditRequested()
        }
    }

    states: [
        State {
            name: "NORMAL"
            when: !root.isSelected && !root.headerHovered &&
                  !leftEar.hovered && !leftStalk.hovered &&
                  !rightEar.hovered && !rightStalk.hovered &&
                  ((root.isLeftLinked || root.isRightLinked) && !root.isLinkedActive)
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
            when: (!root.isSelected &&
                  (leftEar.hovered || leftStalk.hovered) &&
                  !root.headerHovered) ||
                  (root.isLeftLinked && root.isLinkedActive)
            PropertyChanges {
                target: prv
                backgroundColor: ui.blendColors("#ffffff", root.labelColor, 0.7)
                leftEarBackgroundColor: ui.blendColors("#ffffff", root.labelColor, 0.3)
                rightEarBackgroundColor: prv.backgroundColor
            }
        },
        State {
            name: "RIGHT_EAR_HOVERED"
            when: !root.isSelected &&
                  (rightEar.hovered || rightStalk.hovered || (root.isRightLinked && root.isLinkedActive)) &&
                  !root.headerHovered
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
