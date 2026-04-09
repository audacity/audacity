/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui
import Muse.UiComponents

import Audacity.UiComponents

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Zoom toggle (magnifying glass)")

    navigation.direction: NavigationPanel.Both

    property alias zoomPresetModel: preset1Box.model
    property int zoomPreset1: 0
    property int zoomPreset2: 0

    signal zoomPreset1ChangeRequested(int preset)
    signal zoomPreset2ChangeRequested(int preset)

    StyledTextLabel {
        text: qsTrc("appshell/preferences", "A special tool in the top bar that toggles between two different zoom states.")
        width: parent.width
        horizontalAlignment: Text.AlignLeft
    }


    ComboBoxWithTitle {
        id: preset1Box

        title: qsTrc("appshell/preferences", "Zoom state 1:")
        columnWidth: root.columnWidth

        control.textRole: "title"
        control.valueRole: "value"

        currentIndex: control.indexOfValue(root.zoomPreset1)

        navigation.name: "ZoomPreset1Box"
        navigation.panel: root.navigation
        navigation.row: 0
        navigation.column: 0

        onValueEdited: function(newIndex, newValue) {
            root.zoomPreset1ChangeRequested(newValue)
        }
    }

    ComboBoxWithTitle {
        id: preset2Box

        title: qsTrc("appshell/preferences", "Zoom state 2:")
        columnWidth: root.columnWidth

        model: preset1Box.model

        control.textRole: "title"
        control.valueRole: "value"

        currentIndex: control.indexOfValue(root.zoomPreset2)

        navigation.name: "ZoomPreset2Box"
        navigation.panel: root.navigation
        navigation.row: 1
        navigation.column: 0

        onValueEdited: function(newIndex, newValue) {
            root.zoomPreset2ChangeRequested(newValue)
        }
    }
}
