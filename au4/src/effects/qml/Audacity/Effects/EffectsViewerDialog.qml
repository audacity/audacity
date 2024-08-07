/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

StyledDialogView {
    id: root

    property alias id: viewer.id

    title: viewer.title

    contentWidth: content.implicitWidth
    contentHeight: content.implicitHeight

    margins: 16

    Column {
        id: content

        spacing: 16

        FlatButton {
            text: qsTrc("effects", "Presets & settings")
        }

        EffectsViewer {
            id: viewer
        }

        ButtonBox {
            width: parent.width

            buttons: [ ButtonBoxModel.Cancel, ButtonBoxModel.Apply ]

            FlatButton {
                text: qsTrc("effects", "Preview")
                buttonRole: ButtonBoxModel.CustomRole
                buttonId: ButtonBoxModel.CustomButton + 1
                isLeftSide: true

                onClicked: {
                }
            }

            onStandardButtonClicked: function(buttonId) {
                if (buttonId === ButtonBoxModel.Cancel) {
                    root.reject()
                } else if (buttonId === ButtonBoxModel.Apply) {
                    viewer.apply()
                }
            }
        }
    }
}
