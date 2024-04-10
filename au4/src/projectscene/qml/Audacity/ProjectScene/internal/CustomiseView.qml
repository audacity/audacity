/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.ProjectScene 1.0

StyledListView {
    id: root

    spacing: 0
    scrollBarPolicy: ScrollBar.AlwaysOn

    signal selectRowRequested(int index)
    signal clearSelectionRequested()
    signal removeSelectionRequested()

    function positionViewAtSelectedItems() {
        var selectedIndexes = root.model.selectionModel.selectedIndexes
        for (var _index in selectedIndexes) {
            positionViewAtIndex(selectedIndexes[_index].row, ListView.Contain)
        }
    }

    function focusOnFirst() {
        var firstItem = root.itemAtIndex(0)
        if (Boolean(firstItem)) {
            firstItem.navigation.requestActive()
        }
    }

    function clearFocus() {
        root.clearSelectionRequested()
    }

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "CustomiseView"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Both
        onActiveChanged: function(active) {
            if (active) {
                root.forceActiveFocus()
            }
        }

        onNavigationEvent: function(event) {
            if (event.type === NavigationEvent.AboutActive) {
                event.setData("controlName", prv.currentItemNavigationName)
            }
        }
    }

    QtObject {
        id: prv

        property var currentItemNavigationName: []
    }

    delegate: ListItemBlank {
        id: itemDelegate

        property var item: model.item

        height: 38

        isSelected: model.isSelected

        onClicked: {
            root.selectRowRequested(index)
        }

        onRemoveSelectionRequested: {
            root.removeSelectionRequested()
        }

        navigation.name: item.title
        navigation.panel: root.navigationPanel
        navigation.row: model.index
        navigation.column: 0
        navigation.accessible.name: item.title
        navigation.onActiveChanged: {
            if (navigation.active) {
                prv.currentItemNavigationName = navigation.name
                root.positionViewAtIndex(index, ListView.Contain)
            }
        }

        Loader {
            property var delegateType: Boolean(itemDelegate.item) ? itemDelegate.item.type : PlaybackToolBarCustomiseItem.UNDEFINED

            anchors.fill: parent
            sourceComponent: delegateType === PlaybackToolBarCustomiseItem.ACTION ? actionComponent : separatorLineComponent

            Component {
                id: actionComponent

                PlaybackToolBarActionDelegate {
                    item: itemDelegate.item

                    navigationPanel: root.navigationPanel
                    navigationRow: index
                }
            }

            Component {
                id: separatorLineComponent

                StyledTextLabel {
                    anchors.centerIn: parent
                    text: Boolean(itemDelegate.item) ? itemDelegate.item.title : ""
                }
            }
        }
    }
}
