/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.UiComponents

import Audacity.UiComponents 1.0
import Audacity.Playback 1.0
import Audacity.AppShell

BaseSection {
    id: root

    title: qsTrc("preferences", "Meter dB range")
    spacing: 16

    MeterModel {
        id: meterModel
    }

    Component.onCompleted: {
        meterModel.init();
    }

    ComboBoxWithTitle {
        title: qsTrc("preferences", "dB range")

        columnWidth: 320

        currentIndex: meterModel.meterDbRange
        model: meterModel.dbRangeList.map(function(id) {
            return meterModel.description(id);
        })

        navigation.name: "MeterDbRangeBox"
        navigation.panel: root.navigation
        navigation.row: 1

        onValueEdited: function(newIndex, _) {
            meterModel.meterDbRange = newIndex;
        }
    }
}
