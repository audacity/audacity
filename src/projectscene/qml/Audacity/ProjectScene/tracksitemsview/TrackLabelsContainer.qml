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

    onInitRequired: function() {
        labelsModel.init()
    }

    contentComponent: Component {
        Item {
            Item {
                id: labelsContainer
                anchors.fill: parent
                anchors.bottomMargin: root.bottomSeparatorHeight
                z: 1

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

                        height: parent.height
                        width: itemData.width
                        x: itemData.x

                        asynchronous: true

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
                                isSelected: Boolean(itemData) && itemData.selected
                                enableCursorInteraction: true

                                navigation.name: Boolean(itemData) ? itemData.title + itemData.index : ""
                                navigation.panel: root.navigationPanel
                                navigation.column: itemData ? Math.floor(itemData.x) : 0
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
                                    labelsModel.resetSelectedLabel()
                                }

                                onHeaderHoveredChanged: function() {
                                    root.itemHeaderHoveredChanged(headerHovered)
                                }

                                onHoverChanged: function() {
                                    root.hover = labelsContainer.checkIfAnyLabel(function(labelItem) {
                                        return labelItem && labelItem.hover
                                    })
                                }

                                Connections {
                                    target: itemData
                                    function onTitleEditRequested() {
                                        item.editTitle()
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // this one is transparent, it's on top of the labels
            // to have extend/reduce selection area handles
            ObjectsSelection {
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

        function onItemMoveRequested(objectKey, completed) {
            console.info("Labels don't support clip moving yet")
            root.updateMoveActive(completed)
        }

        function onItemStartEditRequested(objectKey) {
            labelsModel.startEditLabel(objectKey)
        }

        function onItemEndEditRequested(objectKey) {
            labelsModel.endEditLabel(objectKey)
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

