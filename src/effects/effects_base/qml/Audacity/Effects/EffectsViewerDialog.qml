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
        }
    }

    ButtonBox {
        id: bbox
        width: parent.width
        anchors.bottom: parent.bottom

        buttons: [ ButtonBoxModel.Cancel, ButtonBoxModel.Apply ]

        onStandardButtonClicked: function(buttonId) {
            switch(buttonId) {
            case ButtonBoxModel.Cancel: root.reject(); break;
            case ButtonBoxModel.Apply: root.accept(); break;
            }
        }

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
    }
}
