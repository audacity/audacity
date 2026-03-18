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

    NavigationPanel {
        id: presetNameNavPanel
        name: "PresetNameInput"
        enabled: input.enabled && input.visible
        section: root.navigationSection
        order: 0
    }

    TextInputField {
        id: input
        anchors.top: parent.top
        anchors.bottom: bbox.top
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottomMargin: 16

        navigation.panel: presetNameNavPanel
        navigation.order: 1
        navigation.name: "Preset name"

        hint: qsTrc("effects", "Preset name")

        onTextChanged: t => input.currentText = t
    }

    ButtonBox {
        id: bbox
        width: parent.width
        anchors.bottom: parent.bottom

        buttons: [ ButtonBoxModel.Cancel, ButtonBoxModel.Ok]
        navigationPanel.section: root.navigationSection
        navigationPanel.order: 2

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
