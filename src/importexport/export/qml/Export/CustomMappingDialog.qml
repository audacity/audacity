/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Muse.Ui
import Muse.UiComponents

import Audacity.Export 1.0
import "internal"

StyledDialogView {
    id: root

    title: qsTrc("export", "Edit mapping")

    contentWidth: 320 - 2 * margins
    contentHeight: 380 - 2 * margins

    modal: true

    margins: 16

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "ChannelMappingPanel"
        section: root.navigationSection
        direction: NavigationPanel.Both
        order: 1
        onActiveChanged: function(active) {
            if (active) {
                root.forceActiveFocus()
            }
        }
    }

    CustomMappingModel {
        id: customMappingModel
    }

    ColumnLayout {
        id: mainLayout

        spacing: 16

        RowLayout {
            StyledTextLabel {
                text: qsTrc("export", "Channel count")
            }

            IncrementalPropertyControl {
                id: channelCountControl

                Layout.preferredWidth: 125

                navigation.panel: root.navigationPanel
                navigation.order: 1

                step: 1
                decimals: 0
                minValue: 1
                maxValue: 32
                currentValue: customMappingModel.exportChannels
                onValueEdited: function (newValue) {
                    customMappingModel.exportChannels = newValue
                }
            }
        }

        ChannelMappingTableView {
            id: mappingTable

            width: 288
            height: 240

            navigationPanel.section: root.navigationSection
            navigationPanel.order: channelCountControl.navigation.order + 1

            channelCount: customMappingModel.exportChannels
        }

        SeparatorLine {}

        ButtonBox {
            id: buttonBox

            Layout.fillWidth: true

            navigationPanel.section: root.navigationSection
            navigationPanel.order: mappingTable.navigationPanel.order + 1

            FlatButton {
                id: cancelBtn
                text: qsTrc("global", "Cancel")
                buttonRole: ButtonBoxModel.RejectRole
                buttonId: ButtonBoxModel.Cancel
                minWidth: 80
                onClicked: {
                    root.reject()
                }
            }

            FlatButton {
                id: okBtn
                text: qsTrc("global", "Apply")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.Apply
                minWidth: 80
                accentButton: true
                onClicked: {
                    mappingTable.commitChanges()
                    customMappingModel.apply()
                    root.accept()
                }
            }
        }
    }
}
