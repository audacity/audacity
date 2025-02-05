/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

StyledDialogView {
    id: root

    property alias type: viewer.type
    property alias instanceId: viewer.instanceId

    title: viewer.title

    contentWidth: Math.max(viewer.implicitWidth, bbox.implicitWidth)
    contentHeight: viewer.implicitHeight + bbox.implicitHeight + 16

    margins: 16

    EffectsViewer {
        id: viewer
        width: parent.width

        onIsApplyAllowedChanged: {
            bbox.buttonById(ButtonBoxModel.Apply).enabled = isApplyAllowed
            bbox.buttonById(previewBtn.buttonId).enabled = isApplyAllowed
        }
    }

    ButtonBox {
        id: bbox
        width: parent.width
        anchors.bottom: parent.bottom

        //! TODO Move function to ButtonBox (Muse framework)
        function buttonById(id) {
            for (var i = 0; i < bbox.count; i++) {
                var btn = bbox.itemAt(i)
                if (btn.buttonId === id) {
                    return btn
                }
            }

            return null
        }

        Component.onCompleted: {
            bbox.buttonById(ButtonBoxModel.Apply).enabled = false
        }

        FlatButton {
            id: manageBtn
            text: qsTrc("effects", "Manage")
            buttonRole: ButtonBoxModel.CustomRole
            buttonId: ButtonBoxModel.CustomButton + 1
            isLeftSide: true
            onClicked: viewer.manage(manageBtn)
        }

        FlatButton {
            id: previewBtn
            text: qsTrc("effects", "Preview")
            buttonRole: ButtonBoxModel.CustomRole
            buttonId: ButtonBoxModel.CustomButton + 2
            isLeftSide: true
            onClicked: viewer.preview()
        }

        FlatButton {
            text: qsTrc("global", "Cancel")
            buttonRole: ButtonBoxModel.RejectRole
            buttonId: ButtonBoxModel.Cancel
            onClicked: root.reject()
        }

        FlatButton {
            text: qsTrc("global", "Apply")
            buttonRole: ButtonBoxModel.AcceptRole
            buttonId: ButtonBoxModel.Apply
            accentButton: true
            onClicked: root.accept()
        }
    }
}
