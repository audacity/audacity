import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

TrackObjectsItem {
    id: root

    signal clipHeaderHoveredChanged(bool value)

    LabelsListModel {
        id: labelsModel
        trackId: root.trackId
        context: root.context
    }

    onInitRequired: function() {
        labelsModel.init()
    }

    contentComponent: Component {
        Item {
            id: labelsContainer
            anchors.fill: parent
            anchors.bottomMargin: root.bottomSeparatorHeight
            z: 1

            Repeater {
                id: repeater
                model: labelsModel

                delegate: Loader {
                    id: labelLoader

                    property QtObject labelItem: model.item
                    property int index: model.index

                    height: parent.height
                    width: labelItem.width
                    x: labelItem.x

                    asynchronous: true

                    sourceComponent: {
                        if ((labelItem.x + labelItem.width) < (0 - labelsModel.cacheBufferPx)) {
                            return null
                        }

                        if (labelItem.x > (labelsContainer.width + labelsModel.cacheBufferPx)) {
                            return null
                        }

                        return labelComp
                    }

                    onLoaded: {
                        labelLoader.item.itemData = model.item
                    }
                }
            }

            Component {
                id: labelComp

                LabelItem {
                    property var itemData: null

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

                    onRequestSelected: {
                        labelsModel.selectLabel(itemData.key)
                        root.labelSelectedRequested()
                    }

                    onRequestSelectionReset: {
                        labelsModel.resetSelectedLabels()
                        root.selectionResetRequested()
                    }

                    onTitleEditAccepted: function(newTitle) {
                        labelsModel.changeLabelTitle(itemData.key, newTitle)
                        labelsModel.resetSelectedClip()
                    }

                    onTitleEditStarted: {
                        labelsModel.selectLabel(itemData.key)
                    }

                    onTitleEditCanceled: {
                        labelsModel.resetSelectedLabel()
                    }

                    onLabelHeaderHoveredChanged: function(headerHovered) {
                        root.clipHeaderHoveredChanged(headerHovered)
                    }
                }
            }

            // MouseArea {
            //     id: labelsContainerMouseArea
            //     anchors.fill: parent
            //     hoverEnabled: true

            //     onPositionChanged: function(e) {
            //         root.trackItemMousePositionChanged(e.x, e.y, null)
            //     }
            // }
        }
    }

    Connections {
        target: root.container

        function onClipMoveRequested(clipKey, completed) {
            // Labels don't support clip moving yet
        }

        function onClipStartEditRequested(clipKey) {
            // Labels don't support this yet
        }

        function onClipEndEditRequested(clipKey) {
            // Labels don't support this yet
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
