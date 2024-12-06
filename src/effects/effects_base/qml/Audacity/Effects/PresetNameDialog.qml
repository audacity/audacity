/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

StyledDialogView {
    id: root

    title: qsTrc("effects", "Save Preset")

    contentWidth: 280
    contentHeight: 80

    margins: 16

    Component.onCompleted: {
        Qt.callLater(input.forceActiveFocus)
    }

    TextInputField {
        id: input
        anchors.top: parent.top
        anchors.bottom: bbox.top
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottomMargin: 16

        hint: qsTrc("effects", "Preset name")

        onTextChanged: t => input.currentText = t
    }

    ButtonBox {
        id: bbox
        width: parent.width
        anchors.bottom: parent.bottom

        buttons: [ ButtonBoxModel.Cancel, ButtonBoxModel.Ok]

        onStandardButtonClicked: function(buttonId) {
            switch (buttonId) {
            case ButtonBoxModel.Cancel:
                root.reject()
                return
            case ButtonBoxModel.Ok:
                root.ret = {
                    errcode: 0,
                    value: input.currentText
                }
                root.hide()
                return

            }
        }
    }
}
