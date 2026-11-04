/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Record

StyledDialogView {
    id: root

    title: qsTrc("record", "Set sound activation level")

    contentWidth: 480
    contentHeight: content.implicitHeight

    resizable: false

    onNavigationActivateRequested: {
        levelControl.navigationPanel.requestActive()
    }

    ColumnLayout {
        id: content

        anchors.fill: parent
        spacing: 0

        SoundActivationLevelControl {
            id: levelControl

            Layout.fillWidth: true

            color: "transparent"
            border.width: 0

            navigationPanel: NavigationPanel {
                name: "SoundActivationLevelPanel"
                direction: NavigationPanel.Horizontal
                section: root.navigationSection
                order: 1
            }
        }

        SeparatorLine {}

        ButtonBox {
            id: buttonBox

            Layout.fillWidth: true
            Layout.margins: 12

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 2

            FlatButton {
                text: qsTrc("global", "Cancel")
                buttonRole: ButtonBoxModel.RejectRole
                buttonId: ButtonBoxModel.Cancel
                minWidth: 80

                onClicked: {
                    root.reject()
                }
            }

            FlatButton {
                text: qsTrc("global", "Apply")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.Apply
                minWidth: 80
                accentButton: true

                onClicked: {
                    levelControl.apply()
                    root.accept()
                }
            }
        }
    }
}
