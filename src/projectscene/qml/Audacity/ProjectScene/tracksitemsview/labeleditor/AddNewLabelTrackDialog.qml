/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

StyledDialogView {
    id: root

    title: qsTrc("projectscene", "New label track")

    contentWidth: 320
    contentHeight: 130

    resizable: false

    AddNewLabelTrackModel {
        id: model
    }

    onNavigationActivateRequested: {
        trackNameInput.navigation.requestActive()
    }

    QtObject {
        id: prv

        function createNewLabelTrackAndClose() {
            var trackName = model.createLabelTrack(trackNameInput.inputField.text);
            if (trackName !== "") {
                root.ret = {errcode: 0, value: trackName}
            }
            root.hide();
        }
    }

    ColumnLayout {
        id: content

        anchors.fill: parent
        spacing: 0

        property NavigationPanel navigationPanel: NavigationPanel {
            name: "NewLabelTrackPanel"
            direction: NavigationPanel.Vertical
            section: root.navigationSection
            order: 1
        }

        StyledTextLabel {
            id: nameLabel

            Layout.fillWidth: true
            Layout.leftMargin: 12
            Layout.rightMargin: 12
            Layout.topMargin: 16

            text: qsTrc("projectscene", "Label track name")
            font: ui.theme.bodyFont
            horizontalAlignment: Text.AlignLeft
        }

        TextInputField {
            id: trackNameInput

            Layout.fillWidth: true
            Layout.leftMargin: 12
            Layout.rightMargin: 12
            Layout.topMargin: 8
            Layout.bottomMargin: 12

            navigation.panel: content.navigationPanel
            navigation.order: 1
            navigation.accessible.name: nameLabel.text + ": " + trackNameInput.currentText

            onAccepted: {
                if (trackNameInput.inputField.text.trim() !== "") {
                    prv.createNewLabelTrackAndClose()
                }
            }
        }

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
                isNarrow: true

                onClicked: {
                    root.hide()
                }
            }

            FlatButton {
                text: qsTrc("projectscene", "Confirm")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.CustomButton
                accentButton: true
                isNarrow: true
                enabled: trackNameInput.inputField.text.trim() !== ""

                onClicked: {
                    prv.createNewLabelTrackAndClose()
                }
            }
        }
    }
}
