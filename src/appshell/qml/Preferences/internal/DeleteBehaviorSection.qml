/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.UiComponents 1.0
import Audacity.TrackEdit 1.0

import "../../shared/internal"

BaseSection {
    id: root

    title: deleteBehaviorPanel.title

    navigationOrderEnd: root.navigation.order

    required property var editPreferencesModel
    property alias parentBackgroundColor: deleteBehaviorPanel.parentBackgroundColor

    DeleteBehaviorPanel {
        id: deleteBehaviorPanel

        width: parent.width

        navigation: root.navigation

        deleteBehavior: editPreferencesModel.deleteBehavior
        closeGapBehavior: editPreferencesModel.closeGapBehavior

        onNewDeleteBehaviorRequested: function (deleteBehavior) {
            editPreferencesModel.setDeleteBehavior(deleteBehavior)
        }

        onNewCloseGapBehaviorRequested: function (closeGapBehavior) {
            editPreferencesModel.setCloseGapBehavior(closeGapBehavior)
        }
    }
}
