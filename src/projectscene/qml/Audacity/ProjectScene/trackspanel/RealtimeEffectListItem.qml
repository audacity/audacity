import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects
import Audacity.ProjectScene

ListItemBlank {
    id: root

    property var item: null
    property var listView: null
    property int index: -1
    property int scrollOffset: 0
    property int topMargin: 0

    property NavigationPanel navigationPanel: null
    property int navigationOrder: 0
    property bool innerNavigationActive: false
    property bool gripReorderActive: false
    property int gripReorderTargetIndex: -1

    property NavigationPanel innerNavigationPanel: NavigationPanel {
        name: prv.title + " controls"
        enabled: root.enabled && root.visible && root.innerNavigationActive
        section: root.navigationPanel ? root.navigationPanel.section : null
        direction: NavigationPanel.Horizontal
        order: root.navigationPanel ? root.navigationPanel.order : 0
    }

    QtObject {
        id: prv
        property string title: root.item ? root.item.effectName() : ""
    }

    // Internal properties
    property int yOffset: 0
    readonly property int animationDuration: 200
    property int itemHeight: listView ? height + listView.spacing : 0

    height: 24
    y: index * itemHeight + yOffset
    clip: false // should be true?
    background.color: "transparent"
    hoverHitColor: "transparent"

    navigation.panel: root.navigationPanel
    navigation.order: root.navigationOrder
    focusBorder.drawOutsideParent: true

    function activateInnerNavigation() {
        if (root.innerNavigationActive) {
            return
        }

        root.innerNavigationActive = true
        gripButton.navigation.requestActive()
    }

    function deactivateInnerNavigation() {
        commitGripReorder()

        if (!root.innerNavigationActive) {
            return
        }

        root.innerNavigationActive = false
        root.gripReorderActive = false
        root.navigation.requestActive()
    }

    function clearReorderPreview() {
        if (!listView) {
            root.yOffset = 0
            root.z = 0
            root.gripReorderTargetIndex = -1
            return
        }

        const siblings = listView.contentItem.children
        for (var i = 0; i < siblings.length; ++i) {
            const sibling = siblings[i]
            if (sibling.hasOwnProperty("yOffset")) {
                sibling.yOffset = 0
            }
        }

        root.yOffset = 0
        root.z = 0
        root.gripReorderTargetIndex = -1
    }

    function previewGripReorder(targetIndex) {
        if (!listView || !listView.model) {
            return
        }

        const boundedTargetIndex = Math.max(0, Math.min(targetIndex, listView.model.count() - 1))
        const siblings = listView.contentItem.children

        root.gripReorderTargetIndex = boundedTargetIndex
        root.yOffset = (boundedTargetIndex - root.index) * itemHeight
        root.z = listView.model.count()

        for (var i = 0; i < siblings.length; ++i) {
            const sibling = siblings[i]
            if (!sibling.hasOwnProperty("yOffset")) {
                continue
            } else if (sibling === root) {
                continue
            } else if (sibling.index > root.index && sibling.index <= boundedTargetIndex) {
                sibling.yOffset = -itemHeight
            } else if (sibling.index < root.index && sibling.index >= boundedTargetIndex) {
                sibling.yOffset = itemHeight
            } else {
                sibling.yOffset = 0
            }
        }
    }

    function targetIndexFromContentPosition() {
        if (!listView) {
            return root.index
        }

        const posInListView = content.mapToItem(listView, 0, itemHeight / 2 - topMargin).y + scrollOffset
        return Math.floor(posInListView / itemHeight)
    }

    function commitGripReorder() {
        if (!listView || !listView.model) {
            return
        }

        const targetIndex = root.gripReorderTargetIndex
        clearReorderPreview()

        if (targetIndex < 0 || targetIndex === root.index) {
            return
        }

        const prevContentY = listView.contentY

        listView.model.moveRow(root.index, targetIndex)
        listView.contentY = prevContentY
    }

    Component.onCompleted: {
        menuModel.init()
    }

    onNavigationTriggered: {
        activateInnerNavigation()
    }

    Behavior on y {
        NumberAnimation {
            id: yAnimation
            duration: animationDuration
            easing.type: Easing.InOutQuad
        }
    }

    RowLayout {
        id: content

        property alias item: root.item
        property alias index: root.index

        width: parent.width
        height: parent.height
        anchors.verticalCenter: parent.verticalCenter
        anchors.horizontalCenter: parent.horizontalCenter

        Drag.active: gripButton.mouseArea.drag.active

        spacing: 4

        states: State {
            when: gripButton.mouseArea.drag.active
            AnchorChanges {
                target: content
                anchors.verticalCenter: undefined
            }
            PropertyChanges {
                target: root
                z: listView.model.count()
            }
        }

        FlatButton {
            id: gripButton

            Layout.preferredWidth: 14
            Layout.preferredHeight: 16
            Layout.alignment: Qt.AlignVCenter
            backgroundRadius: 2

            visible: true
            transparent: !root.gripReorderActive
            hoverHitColor: normalColor
            accentButton: root.gripReorderActive
            accentColor: ui.theme.accentColor
            iconColor: root.gripReorderActive ? ui.theme.extra["white_color"] : ui.theme.fontPrimaryColor
            mouseArea.cursorShape: Qt.SizeAllCursor
            navigation.panel: root.innerNavigationPanel
            navigation.order: 0

            mouseArea.drag.target: content
            mouseArea.drag.axis: Drag.YAxis
            mouseArea.onReleased: {
                root.gripReorderTargetIndex = root.targetIndexFromContentPosition()
                root.commitGripReorder()
            }
            mouseArea.onPositionChanged: {
                if (!mouseArea.drag.active) {
                    return
                }
                root.previewGripReorder(root.targetIndexFromContentPosition())
            }
            mouseArea.drag.minimumY: {
                const itemMiddle = (root.index + 0.5) * itemHeight
                // Pardon the magic number 5 ; it's probably half the top and bottom margin + 1.
                // It allows the drag to stop exactly when the middle of the item meets the top or bottom of the listView.
                return -itemMiddle - 5 + scrollOffset
            }
            mouseArea.drag.maximumY: {
                const itemMiddle = (root.index + 0.5) * itemHeight
                return listView.height - itemMiddle - 5 + scrollOffset
            }

            contentItem: StyledIconLabel {
                iconCode: IconCode.TOOLBAR_GRIP
            }
        }

        BypassEffectButton {
            id: bypassButton

            Layout.margins: 0
            Layout.alignment: Qt.AlignVCenter
            Layout.preferredWidth: root.height
            Layout.minimumHeight: root.height
            Layout.maximumHeight: root.height

            navigation.panel: root.innerNavigationPanel
            navigation.order: gripButton.navigation.order + 1
            navigation.name: "panel bypass btn - " + prv.title

            isMasterEffect: item && item.isMasterEffect
            accentButton: item && item.isActive

            onClicked: {
                item.isActive = item && !item.isActive
            }
        }

        FlatButton {
            id: effectNameButton

            navigation.panel: root.innerNavigationPanel
            navigation.order: bypassButton.navigation.order + 1
            navigation.name: "show ui btn - " + prv.title

            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.preferredWidth: 148

            backgroundItem: RealtimeEffectListItemButtonBackground {
                mouseArea: effectNameButton.mouseArea
                navigationCtrl: effectNameButton.navigation
            }

            StyledTextLabel {
                id: trackNameLabel
                anchors.fill: parent
                anchors.leftMargin: 6
                anchors.rightMargin: 6
                horizontalAlignment: Text.AlignLeft
                verticalAlignment: Text.AlignVCenter
                text: root.item ? root.item.effectName() : ""
            }

            onClicked: {
                // Request navigation activation of the effect panel so that
                // if there already is a dialog open and we click on this button,
                // the new dialog doesn't believe its parent is the previous dialog.
                // Note that calling `navigation.requestActive()` has no effect because
                // the RealtimeEffectSection isn't of `NavigationSection.Exclusive` type.
                if (root.navigationPanel) {
                    root.navigationPanel.requestActive()
                }
                root.item.showEffectDialog()
            }
        }

        FlatButton {
            id: chooseEffectDropdown

            navigation.panel: root.innerNavigationPanel
            navigation.order: effectNameButton.navigation.order + 1
            navigation.name: "replace btn - " + prv.title

            Layout.fillHeight: true
            Layout.preferredWidth: height

            icon: IconCode.SMALL_ARROW_DOWN
            backgroundItem: RealtimeEffectListItemButtonBackground {
                mouseArea: chooseEffectDropdown.mouseArea
                navigationCtrl: chooseEffectDropdown.navigation
            }

            RealtimeEffectListItemMenuModel {
                id: menuModel
                effectState: root.item ? root.item.effectState() : null
                isMasterTrack: root.item && root.item.isMasterEffect
            }

            onClicked: {
                menuModel.load()
                effectMenuLoader.toggleOpened(menuModel)
            }

            StyledMenuLoader {
                id: effectMenuLoader

                onHandleMenuItem: function (menuItem) {
                    menuModel.handleMenuItem(menuItem)
                }
            }
        }
    }

    Connections {
        target: root.navigation

        function onActiveChanged() {
            if (root.navigation.active) {
                root.clearReorderPreview()
                root.innerNavigationActive = false
                root.gripReorderActive = false
            }
        }
    }

    Connections {
        target: gripButton.navigation

        function onActiveChanged() {
            if (!gripButton.navigation.active) {
                Qt.callLater(function() {
                    if (root.gripReorderActive || root.gripReorderTargetIndex >= 0) {
                        root.commitGripReorder()
                    }

                    root.gripReorderActive = false
                })
            }
        }

        function onNavigationEvent(event) {
            switch (event.type) {
            case NavigationEvent.Trigger:
                if (root.gripReorderActive) {
                    root.gripReorderActive = false
                    root.commitGripReorder()
                } else {
                    root.gripReorderActive = true
                    root.gripReorderTargetIndex = root.index
                }
                event.accepted = true
                break
            case NavigationEvent.Up:
                if (!root.gripReorderActive) {
                    return
                }
                root.previewGripReorder((root.gripReorderTargetIndex >= 0 ? root.gripReorderTargetIndex : root.index) - 1)
                event.accepted = true
                break
            case NavigationEvent.Down:
                if (!root.gripReorderActive) {
                    return
                }
                root.previewGripReorder((root.gripReorderTargetIndex >= 0 ? root.gripReorderTargetIndex : root.index) + 1)
                event.accepted = true
                break
            case NavigationEvent.Escape:
                if (root.gripReorderActive) {
                    root.clearReorderPreview()
                    root.gripReorderActive = false
                    event.accepted = true
                    return
                }
                root.deactivateInnerNavigation()
                event.accepted = true
                break
            }
        }
    }

    Connections {
        target: bypassButton.navigation

        function onNavigationEvent(event) {
            if (event.type === NavigationEvent.Escape) {
                root.deactivateInnerNavigation()
                event.accepted = true
            }
        }
    }

    Connections {
        target: effectNameButton.navigation

        function onNavigationEvent(event) {
            if (event.type === NavigationEvent.Escape) {
                root.deactivateInnerNavigation()
                event.accepted = true
            }
        }
    }

    Connections {
        target: chooseEffectDropdown.navigation

        function onNavigationEvent(event) {
            if (event.type === NavigationEvent.Escape) {
                root.deactivateInnerNavigation()
                event.accepted = true
            }
        }
    }
}
