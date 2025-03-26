/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Effect behavior")

    property var editPreferencesModel: null

    navigationOrderEnd: root.navigation.order

    Column {
        width: parent.width
        spacing: 24

        CheckBox {
            id: checkbox

            width: parent.width

            text: qsTrc("appshell/preferences", "Apply effects to all audio when no selection is made")

            checked: editPreferencesModel.applyEffectToAllAudio

            navigation.name: "EmptySelectionEffectsBox"
            navigation.panel: root.navigation

            onClicked: {
                editPreferencesModel.setApplyEffectToAllAudio(!checked)
            }
        }
    }
}
