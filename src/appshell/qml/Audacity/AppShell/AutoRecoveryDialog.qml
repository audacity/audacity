/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.AppShell

StyledDialogView {
    id: root

    objectName: "AutoRecoveryDialog"

    title: qsTrc("appshell/autorecovery", "Automatic crash recovery")

    contentWidth: 640
    contentHeight: 372

    function done(action) {
        root.ret = {
            errcode: 0,
            value: {
                action: action
            }
        }

        root.hide()
    }

    onNavigationActivateRequested: {
        recoverButton.navigation.requestActive()
    }

    AutoRecoveryModel {
        id: recoveryModel

        onAllProjectsDiscarded: {
            root.done("skip")
        }
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: 0

        ColumnLayout {
            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.margins: 16

            spacing: 24

            StyledTextLabel {
                Layout.fillWidth: true

                horizontalAlignment: Text.AlignLeft
                wrapMode: Text.WordWrap

                text: qsTrc("appshell/autorecovery", "The following projects were not saved properly the last time Audacity was run and can be automatically recovered.\n\nAfter recovery, save the projects to ensure changes are written to disk.")
            }

            StyledTableView {
                id: tableView

                Layout.fillWidth: true
                Layout.fillHeight: true

                model: recoveryModel
                headerCapitalization: Font.MixedCase
                // Lets the keyboard, and testflow, reach the "Select" header that inverts the selection
                horizontalHeaderNavigationEnabled: true

                navigationPanel.name: "AutoRecoveryProjects"
                navigationPanel.section: root.navigationSection
                navigationPanel.order: 1

                sourceComponentCallback: function (type) {
                    switch (type) {
                    case AutoRecoveryTableViewCellType.Selected:
                        return selectedComp
                    }
                    return null
                }

                onHorizontalHeaderClicked: function (column) {
                    if (column === 0) {
                        recoveryModel.invertSelection()
                    }
                }

                Component {
                    id: selectedComp

                    CheckBox {
                        id: checkBox

                        property var itemData
                        property var val
                        property int row
                        property int column

                        property NavigationPanel navigationPanel
                        property int navigationRow
                        property int navigationColumnStart

                        property string accessibleName: val ? qsTrc("appshell/autorecovery", "Selected") : qsTrc("appshell/autorecovery", "Not selected")

                        signal changed(string stub)
                        signal editingFinished

                        navigation.panel: navigationPanel
                        navigation.enabled: tableView.currentEditedCell === checkBox
                        navigation.order: navigationRow
                        navigation.column: navigationColumnStart

                        checked: Boolean(val)

                        onClicked: {
                            recoveryModel.toggleSelected(row)
                        }
                    }
                }
            }
        }

        SeparatorLine {}

        RowLayout {
            id: footer

            Layout.fillWidth: true
            Layout.margins: 8

            spacing: 8

            NavigationPanel {
                id: footerNavigationPanel

                name: "AutoRecoveryButtons"

                section: root.navigationSection
                order: tableView.navigationPanel.order + 1
                direction: NavigationPanel.Horizontal
            }

            FlatButton {
                id: quitButton

                minWidth: 80

                navigation.panel: footerNavigationPanel
                navigation.order: 1

                text: qsTrc("appshell/autorecovery", "Quit Audacity")

                onClicked: {
                    root.done("quit")
                }
            }

            Item {
                Layout.fillWidth: true
            }

            FlatButton {
                id: skipButton

                minWidth: 80

                navigation.panel: footerNavigationPanel
                navigation.order: 2

                text: qsTrc("appshell/autorecovery", "Skip")

                onClicked: {
                    root.done("skip")
                }
            }

            FlatButton {
                id: discardButton

                minWidth: 80
                enabled: recoveryModel.hasSelection

                navigation.panel: footerNavigationPanel
                navigation.order: 3

                text: qsTrc("appshell/autorecovery", "Discard selected")

                onClicked: {
                    recoveryModel.discardSelected()
                }
            }

            FlatButton {
                id: recoverButton

                minWidth: 80
                enabled: recoveryModel.hasSelection
                accentButton: true

                navigation.panel: footerNavigationPanel
                navigation.order: 4

                text: qsTrc("appshell/autorecovery", "Recover selected")

                onClicked: {
                    recoveryModel.recoverSelectedAsync()
                    root.done("recover")
                }
            }
        }
    }
}
