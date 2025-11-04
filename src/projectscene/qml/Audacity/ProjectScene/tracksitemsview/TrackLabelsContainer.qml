import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

TrackItemsContainer {
    id: root

    TrackLabelsListModel {
        id: labelsModel
        trackId: root.trackId
        context: root.context
    }

    TrackLabelsLayoutManager {
        id: layoutManager
        labelsModel: labelsModel
    }

    onInitRequired: function() {
        labelsModel.init()
        layoutManager.init()
    }

    contentComponent: Component {
        Item {
            Item {
                id: labelsContainer
                anchors.fill: parent
                anchors.bottomMargin: root.bottomSeparatorHeight
                z: 1
                clip: true

                function mapToAllLabels(e, f) {
                    for (let i = 0; i < repeater.count; i++) {
                        let labelLoader = repeater.itemAt(i)
                        if (labelLoader && labelLoader.item) {
                            let labelPos = labelLoader.mapFromItem(this, e.x, e.y)
                            f(labelLoader.item, {button: e.button, modifiers: e.modifiers, x: labelPos.x, y: labelPos.y})
                        }
                    }
                }

                function checkIfAnyLabel(f) {
                    for (let i = 0; i < repeater.count; i++) {
                        let labelLoader = repeater.itemAt(i)
                        if (labelLoader && labelLoader.item) {
                            if (f(labelLoader.item)) {
                                return true
                            }
                        }
                    }
                    return false
                }

                MouseArea {
                    id: labelsContainerMouseArea
                    anchors.fill: parent

                    propagateComposedEvents: true

                    hoverEnabled: true
                    pressAndHoldInterval: 0

                    cursorShape: root.selectionEditInProgress ? Qt.SizeHorCursor : Qt.IBeamCursor
                    enabled: !root.selectionInProgress

                    onPressed: function(e) {
                        e.accepted = false
                    }

                    onClicked: function(e) {
                        e.accepted = false
                    }

                    onReleased: function(e) {
                        e.accepted = false
                    }

                    onDoubleClicked: function(e) {
                        e.accepted = true
                    }

                    onPositionChanged: function(e) {
                        labelsContainer.mapToAllLabels(e, function(labelItem, mouseEvent) {
                            labelItem.labelItemMousePositionChanged(mouseEvent.x, mouseEvent.y)
                        })
                    }

                    onContainsMouseChanged: function() {
                        labelsContainer.mapToAllLabels({x: mouseX, y: mouseY}, function(labelItem, mouseEvent) {
                            labelItem.setContainsMouse(containsMouse)
                        })
                    }
                }

                Repeater {
                    id: repeater
                    model: labelsModel

                    delegate: Loader {
                        id: labelLoader

                        property var itemData: model.item

                        height: parent.height - y
                        width: itemData.width
                        x: itemData.x
                        y: (itemData.visualHeight + 2) * itemData.level
                        z: itemData.level

                        asynchronous: true

                        visible: y < root.height

                        sourceComponent: {
                            if ((itemData.x + itemData.width) < (0 - labelsModel.cacheBufferPx)) {
                                return null
                            }

                            if (itemData.x > (labelsContainer.width + labelsModel.cacheBufferPx)) {
                                return null
                            }

                            return labelComp
                        }

                        Component {
                            id: labelComp

                            LabelItem {
                                id: item

                                property var itemData: labelLoader.itemData

                                title: Boolean(itemData) ? itemData.title : ""
                                labelColor: Boolean(itemData) ? itemData.color : null
                                labelKey: Boolean(itemData) ? itemData.key : null
                                isSelected: Boolean(itemData) && itemData.selected
                                enableCursorInteraction: true

                                isLeftLinked: Boolean(itemData) && itemData.isLeftLinked
                                isRightLinked: Boolean(itemData) && itemData.isRightLinked
                                isLinkedActive: Boolean(itemData) && itemData.isLinkedActive

                                container: repeater

                                navigation.name: Boolean(itemData) ? itemData.title + itemData.index : ""
                                navigation.panel: root.navigationPanel
                                navigation.column: Boolean(itemData) ? Math.floor(itemData.x) : 0
                                navigation.accessible.name: Boolean(itemData) ? itemData.title : ""
                                navigation.onActiveChanged: {
                                    if (navigation.active) {
                                        root.context.insureVisible(root.context.positionToTime(itemData.x))
                                        root.insureVerticallyVisible(root.y, root.y + root.height)
                                    }
                                }

                                onLabelItemMousePositionChanged: function(xWithinLabel, yWithinLabel) {
                                    var yWithinTrack = yWithinLabel
                                    var xWithinTrack = xWithinLabel + itemData.x

                                    trackItemMousePositionChanged(xWithinTrack, yWithinTrack, itemData.key)
                                }

                                onRequestSelected: {
                                    labelsModel.selectLabel(itemData.key)
                                    root.itemSelectedRequested()
                                }

                                onRequestSelectionReset: {
                                    labelsModel.resetSelectedLabels()
                                    root.selectionResetRequested()
                                }

                                onTitleEditStarted: {
                                    labelsModel.selectLabel(itemData.key)
                                }

                                onTitleEditAccepted: function(newTitle) {
                                    labelsModel.changeLabelTitle(itemData.key, newTitle)
                                    labelsModel.resetSelectedLabels()
                                }

                                onTitleEditCanceled: {
                                    labelsModel.resetSelectedLabels()
                                }

                                onLabelStartEditRequested: function() {
                                    itemData.isEditing = true
                                    labelsModel.startEditItem(itemData.key)
                                }

                                onLabelEndEditRequested: function() {
                                    labelsModel.endEditItem(itemData.key)
                                    itemData.isEditing = false
                                }

                                onLabelLeftStretchRequested: function(unlink, completed) {
                                    var leftLinkedLabelKey = layoutManager.leftLinkedLabel(itemData.key)
                                    labelsModel.stretchLabelLeft(itemData.key, leftLinkedLabelKey, unlink, completed)
                                }

                                onLabelRightStretchRequested: function(unlink, completed) {
                                    var rightLinkedLabelKey = layoutManager.rightLinkedLabel(itemData.key)
                                    labelsModel.stretchLabelRight(itemData.key, rightLinkedLabelKey, unlink, completed)
                                }

                                onHeaderHoveredChanged: function() {
                                    root.itemHeaderHoveredChanged(headerHovered)
                                }

                                onHoverChanged: function() {
                                    root.hover = labelsContainer.checkIfAnyLabel(function(labelItem) {
                                        return labelItem && labelItem.hover
                                    })
                                }

                                onVisualWidthChanged: function() {
                                    itemData.visualWidth = item.visualWidth
                                }

                                onActivateLeftLinkedLabel: {
                                    layoutManager.activateLeftLinkedLabel(itemData.key)
                                }

                                onActivateRightLinkedLabel: {
                                    layoutManager.activateRightLinkedLabel(itemData.key)
                                }

                                onDeactivateLinkedLabel: {
                                    layoutManager.deactivateLinkedLabel(itemData.key)
                                }

                                Connections {
                                    target: labelsModel
                                    function onItemTitleEditRequested(key) {
                                        if (key === item.itemData.key) {
                                            item.editTitle()
                                        }
                                    }
                                }

                                Component.onCompleted: {
                                    itemData.visualHeight = item.headerHeight
                                }
                            }
                        }
                    }
                }
            }

            // this one is transparent, it's on top of the labels
            // to have extend/reduce selection area handles
            ItemsSelection {
                id: labelsSelection

                isDataSelected: root.isDataSelected
                selectionInProgress: root.selectionInProgress
                context: root.context

                anchors.fill: parent
                z: 1

                onSelectionDraged: function(x1, x2, completed) {
                    root.selectionDraged(x1, x2, completed)
                    if (completed) {
                        root.seekToX(Math.min(x1, x2))
                    }
                }

                onRequestSelectionContextMenu: function(x, y) {
                    let position = mapToItem(root.parent, Qt.point(x, y))
                    root.requestSelectionContextMenu(position.x, position.y)
                }

                onHandleGuideline: function(x, completed) {
                    root.handleTimeGuideline(x, completed)
                }
            }
        }
    }

    Connections {
        target: root.container

        function onItemMoveRequested(itemKey, completed) {
            root.updateMoveActive(completed)

            labelsModel.moveSelectedLabels(itemKey, completed)
        }

        function onItemStartEditRequested(itemKey) {
            labelsModel.startEditItem(itemKey)
        }

        function onItemEndEditRequested(itemKey) {
            labelsModel.endEditItem(itemKey)
        }

        function onStartAutoScroll() {
            if (root.context) {
                root.context.startAutoScroll(root.context.mousePositionTime())
            }
        }

        function onStopAutoScroll() {
            if (root.context) {
                root.context.stopAutoScroll()
            }
        }
    }
}

