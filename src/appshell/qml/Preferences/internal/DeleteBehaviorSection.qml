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

    title: qsTrc("appshell/preferences", "Choose behavior when deleting a portion of a clip")

    navigationOrderEnd: root.navigation.order

    property alias editPreferencesModel: deleteBehaviorPanel.editPreferencesModel

    DeleteBehaviorPanel {
        id: deleteBehaviorPanel
        width: parent.width
        navigation: root.navigation
    }
}
