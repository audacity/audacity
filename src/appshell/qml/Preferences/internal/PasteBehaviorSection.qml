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

    Column {
        width: parent.width
        spacing: 16

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

        CheckBox {
            id: checkboxAlwaysPasteAsNew

            width: parent.width

            text: qsTrc("appshell/preferences", "Always paste audio as a new clip")

            checked: editPreferencesModel.pasteAsNewClip

            navigation.name: "PasteAsNewBox"
            navigation.panel: root.navigation
            // first radio button is 0,0
            // navigation.column: 0
            // navigation.row: 1
            // or
            // second radio button is 0,1 but has 2 rows
            // navigation.column: 1
            // navigation.row: 2

            onClicked: {
                editPreferencesModel.setPasteAsNewClip(!checked)
            }
        }
    }
}
