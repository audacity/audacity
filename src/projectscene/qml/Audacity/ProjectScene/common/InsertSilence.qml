import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Playback
import Audacity.ProjectScene

StyledDialogView {
    id: root

    title: qsTrc("projectscene/silence", "Silence")

    contentWidth: 380
    contentHeight: 150

    function preview() {
        silence.preview()
    }

    InsertSilenceModel {
        id: silence
    }

    Component.onCompleted: {
        silence.init()
    }

    GridLayout {
        anchors.fill: parent
        anchors.margins: 20

        rows: 2
        columns: 2

        StyledTextLabel {
            text: qsTrc("projectscene/silence", "Duration:")
        }

        Timecode {
            id: timecode

            Layout.fillHeight: false

            value: silence.duration
            mode: TimecodeModeSelector.Duration
            currentFormatStr: silence.durationFormat
            sampleRate: silence.sampleRate

            onValueChanged: {
                silence.duration = timecode.value
            }

            onCurrentFormatChanged: {
                silence.durationFormat = timecode.currentFormatStr
            }
        }

        ButtonBox {
            Layout.columnSpan: 2
            Layout.fillWidth: true

            buttons: [ ButtonBoxModel.Cancel ]

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 2

            FlatButton {
                text: qsTrc("projectscene/silence", "Generate")
                buttonRole: ButtonBoxModel.CustomRole
                buttonId: ButtonBoxModel.CustomButton + 1

                onClicked: {
                    silence.apply()
                    root.ret = {
                        errcode: 0,
                        value: {
                            duration: silence.duration,
                            durationFormat: silence.durationFormat
                        }
                    }

                    root.hide()
                }
            }

            onStandardButtonClicked: function(buttonId) {
                if (buttonId === ButtonBoxModel.Cancel) {
                    root.reject()
                }
            }
        }
    }
}
