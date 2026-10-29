/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

StyledDialogView {
    id: root

    title: qsTrc("record", "Timed recording")

    contentWidth: 560
    contentHeight: 200

    ColumnLayout {
        anchors.fill: parent

        spacing: 0

        Item {
            Layout.fillWidth: true
            Layout.fillHeight: true
        }

        SeparatorLine {}

        ButtonBox {
            id: buttonBox

            Layout.fillWidth: true
            Layout.margins: 8

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 1

            FlatButton {
                text: qsTrc("global", "Cancel")
                buttonRole: ButtonBoxModel.RejectRole
                buttonId: ButtonBoxModel.Cancel
                isNarrow: true

                onClicked: {
                    root.reject()
                }
            }

            FlatButton {
                text: qsTrc("record", "Start timed recording")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.CustomButton
                accentButton: true
                isNarrow: true
                enabled: false
            }
        }
    }
}
