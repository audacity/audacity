/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.Playback

StyledDialogView {
    id: root

    property real time: 0.0

    contentWidth: 280
    contentHeight: 80

    margins: 16

    Row {
        id: timeRow

        anchors.fill: parent
        spacing: ui.theme.extra.spacing_xl

        Item {

            width: parent.width
            height: 30

            StyledTextLabel {
                id: label

                anchors.verticalCenter: parent.verticalCenter
                anchors.left: parent.left

                text: qsTrc("trackedit/time", "Position:")
            }

            Timecode {
                id: timecode

                anchors.verticalCenter: parent.verticalCenter
                anchors.right: parent.right

                // TODO: get initial value from playback state
                value: 0.0
                mode: TimecodeModeSelector.TimePoint

                onValueChanged: {
                    root.time = timecode.value
                }
            }
        }
    }

    ButtonBox {
        id: bbox
        width: parent.width
        anchors.bottom: parent.bottom

        buttons: [ButtonBoxModel.Cancel, ButtonBoxModel.Ok]

        onStandardButtonClicked: function (buttonId) {
            switch (buttonId) {
            case ButtonBoxModel.Cancel:
                root.reject()
                return
            case ButtonBoxModel.Ok:
                root.ret = {
                    errcode: 0,
                    value: root.time
                }
                root.hide()
                return
            }
        }
    }
}
