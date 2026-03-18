/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects

StyledDialogView {
    id: root

    title: qsTrc("projectscene", "Manage plugins")

    contentWidth: 880
    contentHeight: 528

    onNavigationActivateRequested:
    // TODO
    {}

    onAccessibilityActivateRequested:
    // TODO
    {}

    ColumnLayout {
        anchors.fill: parent
        spacing: 0

        PluginManagerTopPanel {
            id: topPanel

            Layout.fillWidth: true
            Layout.preferredHeight: 48
        }

        PluginManagerTableView {
            id: tableView

            Layout.fillWidth: true
            Layout.fillHeight: true
        }

        SeparatorLine {}

        ButtonBox {
            id: buttonBox

            Layout.fillWidth: true
            Layout.margins: 12

            buttons: [ButtonBoxModel.Close]

            onStandardButtonClicked: function (buttonId) {
                switch (buttonId) {
                case ButtonBoxModel.Close:
                    root.accept()
                    break
                }
            }
        }
    }
}
