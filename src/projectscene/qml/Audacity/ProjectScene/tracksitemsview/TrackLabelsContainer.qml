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

    onInitRequired: function () {
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
                            f(labelLoader.item, {
                                button: e.button,
                                modifiers: e.modifiers,
                                x: labelPos.x,
                                y: labelPos.y
                            })
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

                    cursorShape: Qt.BlankCursor
                    enabled: !root.selectionInProgress

                    // While a selection edit is in progress the cursor on the track body
                    // should indicate a horizontal resize. We reuse SelectionLeft.png as a
                    // generic selection-edit cursor here — the actual left/right edge
                    // direction is handled by ItemsSelection's own handle MouseAreas.
                    readonly property string selectionEditCursor: ":/images/customCursorShapes/SelectionLeft.png"
                    readonly property string idleCursor: ":/images/customCursorShapes/IBeamCursor.png"
                    readonly property int iBeamSize: 26

                    function updateCustomCursor() {
                        if (root.selectionEditInProgress) {
                            CustomCursorProvider.setCursorShape(labelsContainerMouseArea, selectionEditCursor)
                        } else {
                            CustomCursorProvider.setCursorShape(labelsContainerMouseArea, idleCursor, iBeamSize)
                        }
                    }

                    Component.onCompleted: updateCustomCursor()
                    Connections {
                        target: root
                        function onSelectionEditInProgressChanged() {
                            labelsContainerMouseArea.updateCustomCursor()
                        }
                    }

                    onPressed: function (e) {
                        e.accepted = false
                    }

                    onClicked: function (e) {
                        e.accepted = false
                    }

                    onReleased: function (e) {
                        e.accepted = false
                    }

                    onDoubleClicked: function (e) {
                        e.accepted = true
                    }

                    onPositionChanged: function (e) {
                        labelsContainer.mapToAllLabels(e, function (labelItem, mouseEvent) {
                            labelItem.labelItemMousePositionChanged(mouseEvent.x, mouseEvent.y)
                        })
                    }

                    onContainsMouseChanged: function () {
                        labelsContainer.mapToAllLabels({
                            x: mouseX,
                            y: mouseY
                        }, function (labelItem, mouseEvent) {
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
                            if (!itemData.focused) {
                                if ((itemData.x + itemData.width) < (0 - labelsModel.cacheBufferPx)) {
                                    return null
                                }

                                if (itemData.x > (labelsContainer.width + labelsModel.cacheBufferPx)) {
                                    return null
                                }
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
                                isFocused: Boolean(itemData) && itemData.focused

                                selectionInProgress: root.selectionInProgress
                                selectionEditInProgress: root.selectionEditInProgress
                                verticalSelectionEditInProgress: root.verticalSelectionEditInProgress

                                isLeftLinked: Boolean(itemData) && itemData.isLeftLinked
                                isRightLinked: Boolean(itemData) && itemData.isRightLinked
                                isLinkedActive: Boolean(itemData) && itemData.isLinkedActive
                                isPoint: Boolean(itemData) && itemData.isPoint

                                container: repeater

                                navigation.name: Boolean(itemData) ? itemData.key.itemId() : ""
                                navigation.panel: root.navigationPanel
                                navigation.column: Boolean(itemData) ? Math.floor(itemData.x) : 0
                                navigation.accessible.name: Boolean(itemData) ? itemData.title : ""
                                navigation.onActiveChanged: {
                                    if (navigation.highlight) {
                                        root.context.animatedInsureVisible(itemData.time.startTime)
                                        root.insureVerticallyVisible()
                                    }
                                }

                                onLabelItemMousePositionChanged: function (xWithinLabel, yWithinLabel) {
                                    var yWithinTrack = yWithinLabel
                                    var xWithinTrack = xWithinLabel + itemData.x

                                    trackItemMousePositionChanged(xWithinTrack, yWithinTrack, itemData.key)

                                    let time = root.context.findGuideline(root.context.positionToTime(xWithinTrack, true))
                                    root.triggerItemGuideline(time, false)
                                }

                                onRequestSelected: {
                                    labelsModel.selectLabel(itemData.key)
                                }

                                onRequestSingleSelected: {
                                    labelsModel.selectLabel(itemData.key)
                                }

                                onRequestSelectionReset: {
                                    labelsModel.resetSelectedLabels()
                                    root.selectionResetRequested()
                                }

                                onTitleEditStarted: {
                                    itemData.isEditing = true
                                }

                                onTitleEditAccepted: function (newTitle) {
                                    labelsModel.changeLabelTitle(itemData.key, newTitle)
                                    labelsModel.resetSelectedLabels()
                                }

                                onTitleEditCanceled: {
                                    labelsModel.resetSelectedLabels()
                                }

                                onTitleEditFinished: {
                                    itemData.isEditing = false
                                }

                                onLabelStartEditRequested: function () {
                                    itemData.isEditing = true
                                    labelsModel.startEditItem(itemData.key)
                                }

                                onLabelEndEditRequested: function () {
                                    labelsModel.endEditItem(itemData.key)
                                    itemData.isEditing = false
                                }

                                onLabelLeftStretchRequested: function (unlink, completed) {
                                    var leftLinkedLabelKey = layoutManager.leftLinkedLabel(itemData.key)
                                    labelsModel.stretchLabelLeft(itemData.key, leftLinkedLabelKey, unlink, completed)

                                    handleLabelGuideline(itemData.key, Direction.Left, completed)
                                }

                                onLabelRightStretchRequested: function (unlink, completed) {
                                    var rightLinkedLabelKey = layoutManager.rightLinkedLabel(itemData.key)
                                    labelsModel.stretchLabelRight(itemData.key, rightLinkedLabelKey, unlink, completed)

                                    handleLabelGuideline(itemData.key, Direction.Right, completed)
                                }

                                onHeaderHoveredChanged: function () {
                                    root.itemHeaderHoveredChanged(headerHovered)
                                }

                                onHoverChanged: function () {
                                    root.hover = labelsContainer.checkIfAnyLabel(function (labelItem) {
                                        return labelItem && labelItem.hover
                                    })
                                }

                                onVisualWidthChanged: function () {
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
                                    function onItemContextMenuOpenRequested(key) {
                                        if (key === item.itemData.key) {
                                            item.openContextMenu()
                                        }
                                    }
                                }

                                Component.onCompleted: {
                                    itemData.visualHeight = item.headerDefaultHeight
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

                onSelectionResize: function (x1, x2, completed) {
                    root.selectionResize(x1, x2, completed)
                    if (completed) {
                        root.seekToX(Math.min(x1, x2))
                    }
                }

                onRequestSelectionContextMenu: function (x, y) {
                    let position = mapToItem(root.parent, Qt.point(x, y))
                    root.requestSelectionContextMenu(position.x, position.y)
                }

                onHandleGuideline: function (x, completed) {
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

            // Labels neither snap nor show a snapping guideline while being dragged.
            if (typeof labelsModel.findGuideline(itemKey, Direction.Auto) === "number") {
                root.clearItemGuideline()
            }
        }

        function onItemStartEditRequested(itemKey) {
            labelsModel.startEditItem(itemKey)
        }

        function onItemEndEditRequested(itemKey) {
            labelsModel.endEditItem(itemKey)
        }

        function onItemReleaseRequested(itemKey) {
            labelsModel.toggleTracksDataSelectionByLabel(itemKey)
        }

        function onCancelItemDragEditRequested(itemKey) {
            if (labelsModel.cancelItemDragEdit(itemKey)) {
                root.itemDragEditCanceled()
            }
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

    function handleLabelGuideline(labelKey, direction, completed) {
        // itemMoveRequested is broadcast to every track's container, but the guideline is
        // shared across all of them. findGuideline returns undefined when this container
        // doesn't own the dragged item; such containers must not touch the guideline,
        // otherwise they clobber the owning track's guideline. A number means we own the
        // item: a valid time draws the guideline, -1 hides it once out of snapping range.
        let time = labelsModel.findGuideline(labelKey, direction)
        if (typeof time !== "number") {
            return
        }
        triggerItemGuideline(time, completed)
    }
}
