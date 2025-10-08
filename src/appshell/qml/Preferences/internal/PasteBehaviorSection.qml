/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.TrackEdit 1.0

import "../../shared/internal"

BaseSection {
    id: root

    title: pasteBehaviorPanel.title

    navigationOrderEnd: root.navigation.order

    required property var editPreferencesModel
    property alias parentBackgroundColor: pasteBehaviorPanel.parentBackgroundColor

    PasteBehaviorPanel {
        id: pasteBehaviorPanel

        width: parent.width

        navigation: root.navigation

        pasteBehavior: editPreferencesModel.pasteBehavior
        pasteInsertBehavior: editPreferencesModel.pasteInsertBehavior

        onNewPasteBehaviorRequested: function (pasteBehavior) {
            editPreferencesModel.setPasteBehavior(pasteBehavior)
        }

        onNewPasteInsertBehaviorRequested: function (pasteInsertBehavior) {
            editPreferencesModel.setPasteInsertBehavior(pasteInsertBehavior)
        }
    }
}
