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

    title: qsTrc("effects", "Manage plugins")

    contentWidth: tableViewModel.totalWidth
    contentHeight: 528
    margins: 12

    Component.onDestruction: tableViewModel.aboutToDestroy()

    onNavigationActivateRequested: {
        topPanel.focusOnFirst()
    }

    onAccessibilityActivateRequested: {
        topPanel.readInfo()
    }

    property PluginManagerTableViewModel tableViewModel: PluginManagerTableViewModel {}

    ColumnLayout {
        anchors.fill: parent
        spacing: 12

        PluginManagerTopPanel {
            id: topPanel

            tableViewModel: root.tableViewModel

            Layout.fillWidth: true
            Layout.preferredHeight: topPanel.contentHeight

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 1

            onSearchTextChanged: function (newText) {
                root.tableViewModel.setSearchText(newText)
            }
        }

        PluginManagerTableView {
            id: tableView

            tableViewModel: root.tableViewModel

            Layout.fillWidth: true
            Layout.fillHeight: true

            navigationPanel.section: root.navigationSection
            navigationPanel.order: topPanel.navigationPanel.order + 1
        }

        RowLayout {
            id: buttonBox

            spacing: 12

            Layout.fillWidth: true
            Layout.margins: 0

            NavigationPanel {
                id: buttonBoxNavigationPanel

                section: root.navigationSection
                order: tableView.navigationPanel.order + 1
            }

            FlatButton {
                id: rescanButton

                Layout.alignment: Qt.AlignVCenter
                Layout.preferredHeight: 32
                Layout.preferredWidth: implicitWidth

                navigation.panel: buttonBoxNavigationPanel
                navigation.order: cancelButton.navigation.order + 1

                text: qsTrc("effects", "Rescan plugins")

                onClicked: {
                    root.tableViewModel.rescanPlugins()
                }
            }

            CheckBox {
                id: alsoRescanBrokenPluginsCheckBox

                Layout.alignment: Qt.AlignVCenter
                Layout.preferredWidth: implicitWidth

                navigation.panel: buttonBoxNavigationPanel
                navigation.order: rescanButton.navigation.order + 1

                text: qsTrc("effects", "Include plugins with errors")
                checked: root.tableViewModel.alsoRescanBrokenPlugins

                onClicked: {
                    root.tableViewModel.alsoRescanBrokenPlugins = !checked
                }
            }

            Item {
                id: spacer
                Layout.fillWidth: true
            }

            FlatButton {
                id: okButton

                Layout.alignment: Qt.AlignVCenter
                Layout.preferredHeight: 32
                Layout.preferredWidth: implicitWidth

                navigation.panel: buttonBoxNavigationPanel
                navigation.order: 0

                text: qsTrc("effects", "OK")

                onClicked: {
                    root.tableViewModel.accept()
                    root.accept()
                }
            }

            FlatButton {
                id: cancelButton

                Layout.alignment: Qt.AlignVCenter
                Layout.preferredHeight: 32
                Layout.preferredWidth: implicitWidth

                navigation.panel: buttonBoxNavigationPanel
                navigation.order: okButton.navigation.order + 1

                text: qsTrc("effects", "Cancel")

                onClicked: {
                    root.reject()
                }
            }
        }
    }
}
