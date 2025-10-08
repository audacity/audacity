import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

TrackObjectsItem {

    id: root

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
                    property QtObject labelItem: model.item
                    property int index: model.index

                    height: parent.height
                    width: Math.max(3, labelItem.width)
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
                }
            }

            Component {
                id: labelComp

                Rectangle {
                    color: "lightblue"
                    border.color: "blue"
                    border.width: 1

                    Text {
                        anchors.centerIn: parent
                        text: labelItem.title
                        color: "black"
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
