/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.Export 1.0
import "internal"

StyledDialogView {
    id: root

    title: qsTrc("export", "Metadata editor")

    contentWidth: 880
    contentHeight: 625

    modal: true
    alwaysOnTop: true

    property NavigationPanel navigation: NavigationPanel {
        name: root.title
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.navigationSection
    }
    property int navigationOrder: 0

    MetadataModel {
        id: metadataModel
    }

    Component.onCompleted: {
        metadataModel.load()
    }

    ColumnLayout {
        id: mainColumn

        spacing: 0

        MetadataControlPanel {
            id: topButtonsBar

            metadataModel: metadataModel
            tagView: metadataView

            navigationPanel: root.navigation
            navigationOrder: root.navigationOrder
        }

        ValueList {
            id: metadataView

            keyRoleName: "tag"
            keyTitle: qsTrc("metadata", "tag")
            valueRoleName: "value"
            valueTitle: qsTrc("metadata", "value")
            valueTypeRole: "roleType"
            readOnly: false

            width: root.contentWidth
            height: 527

            drawZebra: false
            headerColor: ui.theme.backgroundSecondaryColor
            keyColumnWidth: 140
            isKeyEditable: true
            headerCapitalization: Font.Capitalize
            startEditByDoubleClick: true

            navigationSection: navigation.section
            navigationOrderStart: topButtonsBar.navigationOrder + 1

            model: metadataModel

            onKeyEdited: function(row, newKey) {
                metadataModel.renameTag(row, newKey)
            }

            onValueEdited: function(row, newVal) {
                metadataModel.setTagValue(row, newVal)
            }
        }

        ButtonBox {
            id: buttonBox

            Layout.fillWidth: true

            padding: 8

            navigationPanel.section: root.navigation.section
            navigationPanel.order: metadataView.navigationOrderStart + 2

            FlatButton {
                id: cancelBtn

                minWidth: 80

                text: qsTrc("global", "Close")
                buttonRole: ButtonBoxModel.RejectRole
                buttonId: ButtonBoxModel.Cancel

                onClicked: root.reject()
            }

            FlatButton {
                id: okBtn

                minWidth: 80

                text: qsTrc("global", "Apply")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.Apply
                accentButton: true

                onClicked: {
                    root.accept()
                    metadataModel.apply()
                }
            }
        }
    }
}
