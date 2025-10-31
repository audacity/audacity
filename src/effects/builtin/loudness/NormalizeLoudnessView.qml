import QtQuick
import Muse.UiComponents
import Audacity.Effects
import Audacity.BuiltinEffects

BuiltinEffectBase {
    id: root

    property string title: normalizeLoudness.effectTitle
    property bool isApplyAllowed: true

    width: 400
    implicitHeight: column.height

    builtinEffectModel: NormalizeLoudnessViewModelFactory.createModel(root, root.instanceId)
    property alias normalizeLoudness: root.builtinEffectModel

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
                currentIndex: normalizeLoudness.useRmsAlgorithm ? 0 : 1
                onActivated: function (index) {
                    normalizeLoudness.useRmsAlgorithm = index === 0
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
                minValue: normalizeLoudness.targetMin
                maxValue: normalizeLoudness.targetMax
                currentValue: normalizeLoudness.useRmsAlgorithm ? normalizeLoudness.rmsTarget : normalizeLoudness.perceivedLoudnessTarget
                onValueEdited: function (newValue) {
                    if (normalizeLoudness.useRmsAlgorithm) {
                        normalizeLoudness.rmsTarget = newValue
                    } else {
                        normalizeLoudness.perceivedLoudnessTarget = newValue
                    }
                }
            }
        }

        Row {
            spacing: 8

            CheckBox {
                checked: normalizeLoudness.normalizeStereoChannelsIndependently
                onClicked: {
                    normalizeLoudness.normalizeStereoChannelsIndependently = !checked
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
                enabled: !normalizeLoudness.useRmsAlgorithm
                checked: normalizeLoudness.useDualMono
                onClicked: {
                    normalizeLoudness.useDualMono = !checked
                }
            }

            StyledTextLabel {
                anchors.verticalCenter: parent.verticalCenter
                text: normalizeLoudness.useDualMonoLabel
            }
        }
    }
}
