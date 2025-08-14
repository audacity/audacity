/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Preferences

import Audacity.TrackEdit 1.0
import Audacity.Preferences 1.0

StyledDialogView {
    id: root

    contentWidth: 880
    contentHeight: 880

    NavigationPanel {
        id: navigationPanel
        section: root.navigationSection
    }

    EditPreferencesModel {
        id: editPreferencesModel
    }

    DeleteBehaviorPanel {
        id: deleteBehaviorPanel
        width: parent.width
        navigation: navigationPanel
        editPreferencesModel: editPreferencesModel
    }
}
