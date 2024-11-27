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

    property alias preferredHeight: trackEffectList.height

    visible: trackEffectList.count > 0

    Component.onCompleted: {
            trackEffectListModel.load()
        }

    RtEffectListModel {
        id: trackEffectListModel
        trackId: view.itemAtIndex(effectsPanel.selectedTrackIndex).item.trackId
    }

    StyledListView {
        id: trackEffectList
        width: parent.width
        height: contentItem.height
        spacing: 8
        cacheBuffer: 3000
        ScrollBar.vertical: null
        interactive: true
        model: trackEffectListModel

        delegate: RealtimeEffectListItem {
            item: itemData
            height: 24
            availableEffects: trackEffectList.model.availableEffects
            handleMenuItemWithState: trackEffectList.model.handleMenuItemWithState
        }
    }
}
