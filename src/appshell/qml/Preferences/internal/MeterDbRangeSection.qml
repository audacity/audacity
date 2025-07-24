/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.UiComponents 1.0
import Audacity.Playback 1.0
import Audacity.Preferences

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Meter dB range")
    spacing: 16

    property var model: null

    PlaybackStateModel {
        id: playbackState
    }

    ComboBoxWithTitle {
        title: qsTrc("appshell/preferences", "dB range")

        columnWidth: 320

        currentIndex: root.model.meterDbRange
        model: playbackMeterModel.dbRanges.map(function(item) {
            return item.title;
        })

        navigation.name: "MeterDbRangeBox"
        navigation.panel: root.navigation
        navigation.row: 1

        onValueEdited: function(newIndex, newValue) {
            root.model.meterDbRange = newIndex;
        }
    }
}
