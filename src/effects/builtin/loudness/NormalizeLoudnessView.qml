import QtQuick
import Muse.UiComponents
import Audacity.Effects

import "../common"

EffectBase {
    id: root

    property string title: qsTrc("effects/loudness", "Normalize loudness")
    property bool isApplyAllowed: true

    width: 400
    implicitHeight: column.height

    model: normalizeLoudness

    NormalizeLoudnessViewModel {
        id: normalizeLoudness

        instanceId: root.instanceId
    }

    Column {
        id: column

        spacing: 16
        bottomPadding: 16

        Row {
            spacing: 6

            StyledTextLabel {
                anchors.verticalCenter: parent.verticalCenter
                text: qsTrc("effects/loudness", "Normalize")
            }

            StyledDropdown {
                id: algorithmDropdown

                width: 194
                anchors.verticalCenter: parent.verticalCenter

                model: [qsTrc("effects/loudness", "RMS"), qsTrc("effects/loudness", "Perceived loudness")]
                currentIndex: root.model.useRmsAlgorithm ? 0 : 1
                onActivated: function(index) {
                    root.model.useRmsAlgorithm = index === 0;
                }
            }

            StyledTextLabel {
                anchors.verticalCenter: parent.verticalCenter
                text: qsTrc("effects/loudness", "to")
            }

            IncrementalPropertyControl {
                width: 125
                anchors.verticalCenter: parent.verticalCenter

                measureUnitsSymbol: root.model.useRmsAlgorithm ? "dB" : "LUFS"
                step: 1
                decimals: 1
                minValue: root.model.targetMin
                maxValue: root.model.targetMax
                currentValue: root.model.useRmsAlgorithm ? root.model.rmsTarget : root.model.perceivedLoudnessTarget
                onValueEdited: function(newValue) {
                    if (root.model.useRmsAlgorithm) {
                        root.model.rmsTarget = newValue;
                    } else {
                        root.model.perceivedLoudnessTarget = newValue;
                    }
                }
            }
        }

        Row {
            spacing: 8

            CheckBox {
                checked: root.model.normalizeStereoChannelsIndependently
                onClicked: {
                    root.model.normalizeStereoChannelsIndependently = !checked;
                }
            }

            StyledTextLabel {
                anchors.verticalCenter: parent.verticalCenter
                text: qsTrc("effects/loudness", "Normalize stereo channels independently")
            }
        }

        Row {
            spacing: 8

            CheckBox {
                enabled: !root.model.useRmsAlgorithm
                checked: root.model.dualMono
                onClicked: {
                    root.model.dualMono = !checked;
                }
            }

            StyledTextLabel {
                anchors.verticalCenter: parent.verticalCenter
                text: qsTrc("effects/loudness", "Treat mono as dual mono (recommended)")
            }
        }
    }

    Component.onCompleted: {
        root.model.init()
    }
}
