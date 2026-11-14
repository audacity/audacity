import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.Cloud

StyledDialogView {
    id: root

    title: qsTrc("project/cloud", "Syncing with audio.com")

    contentWidth: 367
    contentHeight: content.implicitHeight

    margins: prv.contentSpacing

    QtObject {
        id: prv

        readonly property int contentSpacing: 16
        readonly property int columnSpacing: 16
        readonly property int buttonBoxMargins: 8
    }

    onNavigationActivateRequested: {
        buttonBox.focusOnFirstEnabled()
    }

    CloudProjectSyncModel {
        id: model

        onSyncingChanged: {
            if (!model.syncing) {
                root.ret = {
                    errcode: 0,
                    value: "synced"
                }
                root.hide()
            }
        }
    }

    Component.onCompleted: {
        model.load()
    }

    Column {
        id: content

        width: parent.width
        spacing: prv.columnSpacing

        StyledTextLabel {
            width: parent.width

            text: qsTrc("project/cloud", "Project is syncing with audio.com. Do you want to stop the sync process?")

            font: ui.theme.bodyFont
            horizontalAlignment: Text.AlignLeft
            wrapMode: Text.Wrap
        }

        ButtonBox {
            id: buttonBox

            width: parent.width

            buttons: [ButtonBoxModel.Cancel]

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 1

            FlatButton {
                text: qsTrc("global", "Stop")
                buttonRole: ButtonBoxModel.AcceptRole
                accentButton: true

                onClicked: {
                    root.ret = {
                        errcode: 0,
                        value: "stopped"
                    }
                    root.hide()
                }
            }

            onStandardButtonClicked: function (buttonId) {
                if (buttonId === ButtonBoxModel.Cancel) {
                    root.ret = {
                        errcode: 0,
                        value: "canceled"
                    }
                    root.hide()
                }
            }
        }
    }
}
