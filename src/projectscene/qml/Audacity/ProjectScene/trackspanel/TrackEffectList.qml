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
    property alias trackName: trackEffectListModel.trackName
    property alias trackEffectsActive: trackEffectListModel.trackEffectsActive
    property alias showEffectsSection: trackEffectListModel.showEffectsSection

    Component.onCompleted: {
        trackEffectListModel.load()
    }

    RealtimeEffectListModel {
        id: trackEffectListModel
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
