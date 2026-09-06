/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import Muse.Ui

import "../../qml/Audacity/Preferences/internal"

Item {
    id: root
    width: 640
    height: 640
    property bool closeRequested: false

    NavigationSection {
        id: preferencesNavigation
        name: "PreferencesTestWindow"
        order: 1
        type: NavigationSection.Exclusive
        onNavigationEvent: function (event) {
            if (event.type === NavigationEvent.Escape) {
                root.closeRequested = true
            }
        }
    }

    AudioApiSection {
        apiModel: recordingApiModel
        audioApiList: recordingApiModel.audioApiList()

        navigation.name: "AudioApiSection"
        navigation.section: preferencesNavigation
        navigation.order: 1
    }
}
