import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

StyledDialogView {
    id: root

    property string trackId: ""
    property string clipId: ""

    property string focusItemName: "" // pitch, speed

    title: qsTrc("projectscene", "Pitch and speed") + " - " + changeModel.clipTitle

    contentWidth: 320
    contentHeight: 352

    modal: true
    resizable: false
    alwaysOnTop: true

    PitchAndSpeedChangeModel {
        id: changeModel

        onCloseDialogRequested: function(){
            root.close()
        }
    }

    Component.onCompleted: {
        changeModel.load(root.trackId, root.clipId)
    }

    onNavigationActivateRequested: {
        if (root.focusItemName == "speed") {
            speedSection.requestActiveFocus()
        } else {
            pitchSection.requestActiveFocus()
        }
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: 0

        Column {
            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.margins: 16

            spacing: 24

            PitchSection {
                id: pitchSection

                width: parent.width

                pitch: changeModel.pitch

                navigationPanel.section: root.navigationSection
                navigationPanel.order: 1

                onValueChanged: function(newValue){
                    changeModel.pitch = newValue
                }
            }

            SpeedSection {
                id: speedSection

                width: parent.width

                speedPercentage: changeModel.speedPercentage
                canChange: changeModel.canChangeSpeed

                navigationPanel.section: root.navigationSection
                navigationPanel.order: 2

                onValueChanged: function(newValue){
                    changeModel.speedPercentage = newValue
                }
            }

            GeneralSection {
                width: parent.width

                optimizeForVoice: changeModel.optimizeForVoice

                navigationPanel.section: root.navigationSection
                navigationPanel.order: 3

                onValueChanged: function(newValue){
                    changeModel.optimizeForVoice = newValue
                }
            }
        }

        SeparatorLine {}

        ButtonBox {
            id: buttonBox

            Layout.fillWidth: true
            Layout.margins: 12

            buttons: [ ButtonBoxModel.Done ]

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
