/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Rectangle {
    id: root
    implicitHeight: trackEffectList.contentHeight
    readonly property int topMargin: 8
    property alias isMasterTrack: trackEffectListModel.isMasterTrack
    property alias trackName: trackEffectListModel.trackName
    property alias trackEffectsActive: trackEffectListModel.trackEffectsActive
    property bool empty: trackEffectList.count == 0
    property alias count: trackEffectList.count
    property NavigationPanel navigationPanel: null
    property int navigationOrderStart: 0
    property int pendingGripFocusIndex: -1

    Component.onCompleted: {
        trackEffectListModel.load()
    }

    function focusGripAtIndex(targetIndex) {
        const delegate = trackEffectList.itemAtIndex(targetIndex)
        if (!delegate) {
            return
        }

        delegate.innerNavigationActive = true
        delegate.gripReorderActive = false
        delegate.gripControl.navigation.requestActive()
    }

    RealtimeEffectListModel {
        id: trackEffectListModel
    }

    Component {
        id: listMargin
        Item {
            height: root.topMargin
        }
    }

    StyledListView {
        id: trackEffectList
        anchors.fill: parent
        spacing: 6
        cacheBuffer: 3000
        interactive: trackEffectList.contentHeight > trackEffectList.height
        model: trackEffectListModel
        boundsBehavior: Flickable.StopAtBounds
        boundsMovement: Flickable.FollowBoundsBehavior
        flickDeceleration: 10000
        footer: listMargin
        header: listMargin

        clip: false
        anchors.margins: 0

        delegate: RealtimeEffectListItem {
            item: itemData
            scrollOffset: trackEffectList.contentY + root.topMargin
            topMargin: root.topMargin
            index: model.index
            listView: trackEffectList
            width: trackEffectList.width - scrollbarContainer.width
            navigationPanel: root.navigationPanel
            navigationOrder: root.navigationOrderStart + model.index

            onGripReorderCommitted: function(targetIndex) {
                root.pendingGripFocusIndex = targetIndex
            }
        }

        ScrollBar.vertical: scrollbar

        Item {
            id: scrollbarContainer
            width: 12
            height: parent.height
            anchors.right: parent.right

            StyledScrollBar {
                id: scrollbar
                anchors.fill: parent
                thickness: 5
                policy: ScrollBar.AlwaysOn
            }
        }
    }

    Connections {
        target: trackEffectListModel

        function onLayoutChanged() {
            if (root.pendingGripFocusIndex < 0) {
                return
            }

            const targetIndex = root.pendingGripFocusIndex
            root.pendingGripFocusIndex = -1

            Qt.callLater(function() {
                root.focusGripAtIndex(targetIndex)
            })
        }
    }
}
