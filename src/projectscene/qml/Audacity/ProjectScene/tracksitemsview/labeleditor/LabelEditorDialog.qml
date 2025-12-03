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

    title: qsTrc("projectscene", "Label editor")

    contentWidth: 880
    contentHeight: 528

    resizable: false

    // PitchAndSpeedChangeModel {
    //     id: changeModel

    //     onCloseDialogRequested: function(){
    //         root.close()
    //     }
    // }

    // Component.onCompleted: {
    //     changeModel.load(root.trackId, root.clipId)
    // }

    // onNavigationActivateRequested: {
    //     if (root.focusItemName == "speed") {
    //         speedSection.requestActiveFocus()
    //     } else {
    //         pitchSection.requestActiveFocus()
    //     }
    // }

    ColumnLayout {
        anchors.fill: parent
        spacing: 0

        LabelEditorTopPanel {
            Layout.fillWidth: true
        }

        SeparatorLine {}

        LabelEditorLabelsTableView {
            Layout.fillWidth: true
            Layout.fillHeight: true
        }

        SeparatorLine {}

        ButtonBox {
            id: buttonBox

            Layout.fillWidth: true
            Layout.margins: 12

            buttons: [ ButtonBoxModel.Close ]

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 4

            onStandardButtonClicked: function(buttonId) {
                switch(buttonId) {
                case ButtonBoxModel.Done: root.accept(); break;
                }
            }
        }
    }
}
