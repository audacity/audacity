import QtQuick
import Muse.UiComponents
import Audacity.Effects

import "../common"

BuiltinEffectBase {
    id: root

    property string title: normalizeLoudness.effectTitle
    property bool isApplyAllowed: true

    width: 400
    implicitHeight: column.height

    model: normalizeLoudness

    NormalizeLoudnessViewModel {
        id: normalizeLoudness
    }

    Column {
        id: column

        spacing: 16
        bottomPadding: 16

        Row {
            spacing: 6

            StyledTextLabel {
                anchors.verticalCenter: parent.verticalCenter
                text: normalizeLoudness.normalizeLabel
            }

            StyledDropdown {
                id: algorithmDropdown

                width: 194
                anchors.verticalCenter: parent.verticalCenter

                model: normalizeLoudness.algorithmOptions
                currentIndex: root.model.useRmsAlgorithm ? 0 : 1
                onActivated: function (index) {
                    root.model.useRmsAlgorithm = index === 0
                }
            }

            StyledTextLabel {
                anchors.verticalCenter: parent.verticalCenter
                text: normalizeLoudness.toLabel
            }

            IncrementalPropertyControl {
                width: 125
                anchors.verticalCenter: parent.verticalCenter

                measureUnitsSymbol: normalizeLoudness.currentMeasureUnitsSymbol
                step: normalizeLoudness.targetStep
                decimals: normalizeLoudness.targetDecimals
                minValue: root.model.targetMin
                maxValue: root.model.targetMax
                currentValue: root.model.useRmsAlgorithm ? root.model.rmsTarget : root.model.perceivedLoudnessTarget
                onValueEdited: function (newValue) {
                    if (root.model.useRmsAlgorithm) {
                        root.model.rmsTarget = newValue
                    } else {
                        root.model.perceivedLoudnessTarget = newValue
                    }
                }
            }
        }

        Row {
            spacing: 8

            CheckBox {
                checked: root.model.normalizeStereoChannelsIndependently
                onClicked: {
                    root.model.normalizeStereoChannelsIndependently = !checked
                }
            }

            StyledTextLabel {
                anchors.verticalCenter: parent.verticalCenter
                text: normalizeLoudness.independentStereoLabel
            }
        }

        Row {
            spacing: 8

            CheckBox {
                enabled: !root.model.useRmsAlgorithm
                checked: root.model.useDualMono
                onClicked: {
                    root.model.useDualMono = !checked
                }
            }

            StyledTextLabel {
                anchors.verticalCenter: parent.verticalCenter
                text: normalizeLoudness.useDualMonoLabel
            }
        }
    }

    Component.onCompleted: {
        root.model.init()
    }
}
