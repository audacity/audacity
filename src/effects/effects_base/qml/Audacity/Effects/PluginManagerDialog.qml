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

    property PluginManagerTableViewModel tableViewModel: PluginManagerTableViewModel {}

    ColumnLayout {
        anchors.fill: parent
        spacing: 0

        PluginManagerTopPanel {
            id: topPanel

            tableViewModel: root.tableViewModel

            Layout.fillWidth: true
            Layout.preferredHeight: 48

            onSearchTextChanged: function (newText) {
                root.tableViewModel.setSearchText(newText)
            }
        }

        PluginManagerTableView {
            id: tableView

            tableViewModel: root.tableViewModel

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
