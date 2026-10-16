/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick

import Muse.UiComponents

import Audacity.UiComponents 1.0

BaseSection {
    id: root

    title: "WASAPI"
    spacing: 16

    property var apiModel: null

    ComboBoxWithTitle {
        title: qsTrc("preferences", "Mode")
        columnWidth: root.columnWidth

        currentIndex: indexOfValue(apiModel.wasapiMode)
        textRole: "text"
        valueRole: "value"
        model: [
            {
                text: qsTrc("preferences", "Shared"),
                value: "shared"
            },
            {
                text: qsTrc("preferences", "Shared low latency"),
                value: "shared-low-latency"
            },
            {
                text: qsTrc("preferences", "Exclusive"),
                value: "exclusive"
            }
        ]

        navigation.name: "WasapiModeBox"
        navigation.panel: root.navigation
        navigation.row: 1

        onValueEdited: function (newIndex, newValue) {
            apiModel.setWasapiMode(newValue)
        }
    }

    StyledTextLabel {
        width: parent.width
        horizontalAlignment: Text.AlignLeft
        wrapMode: Text.WordWrap

        text: apiModel.wasapiMode === "exclusive" ? qsTrc("preferences", "Other applications cannot use the device. System playback recording requires shared mode.") : apiModel.wasapiMode === "shared-low-latency" ? qsTrc("preferences", "Other applications can use the device. A shorter audio period is used when the driver supports it.") : qsTrc("preferences", "Other applications can use the device.")
    }
}
