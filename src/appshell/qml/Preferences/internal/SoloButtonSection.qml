/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.Playback

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Solo button behavior")

    navigation.direction: NavigationPanel.Vertical

    property var playbackPreferencesModel: null

    columnSpacing: 16

    RadioButtonGroup {
        id: soloBtnGroup

        spacing: root.rowSpacing
        orientation: Qt.Vertical

        width: parent.width
        height: 55

        Column {
            width: parent.width
            spacing: root.columnSpacing

            RoundedRadioButton {
                width: parent.width

                checked: playbackPreferencesModel.soloBehavior == SoloBehavior.SoloBehaviorMulti
                text: qsTrc("appshell/preferences", "Solo can be activated for multiple tracks at the same time")

                navigation.name: "SoloBehaviorMultiBox"
                navigation.panel: root.navigation
                navigation.row: 0

                onToggled: {
                    playbackPreferencesModel.setSoloBehavior(SoloBehavior.SoloBehaviorMulti)
                }
            }

            RoundedRadioButton {
                width: parent.width

                checked: playbackPreferencesModel.soloBehavior == SoloBehavior.SoloBehaviorSimple
                text: qsTrc("appshell/preferences", "When solo is activated, it deactivates solo for all other tracks")

                navigation.name: "SoloBehaviorSimpleBox"
                navigation.panel: root.navigation
                navigation.row: 1

                onToggled: {
                    playbackPreferencesModel.setSoloBehavior(SoloBehavior.SoloBehaviorSimple)
                }
            }
        }
    }
}
