import QtQuick
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects

import "../common"

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects/normalize", "Normalize")
    property bool isApplyAllowed: removeDcCheckbox.checked || normalizePeakAmplitudeCheckbox.checked

    width: 400
    implicitHeight: column.height

    model: normalize

    NormalizeViewModel {
        id: normalize
    }

    Component.onCompleted: {
        normalize.init()
    }

    Column {
        id: column

        topPadding: 2
        bottomPadding: 2
        spacing: 10

        CheckBox {
            id: removeDcCheckbox

            text: qsTrc("effects/normalize", "Remove DC offset (center on 0.0 vertically)")

            onClicked: {
                normalize.removeDC = !checked
            }
            checked: normalize.removeDC
        }

        RowLayout {
            width: root.width
            height: 28

            CheckBox {
                id: normalizePeakAmplitudeCheckbox

                Layout.alignment: Qt.AlignVCenter | Qt.AlignLeft
                Layout.fillWidth: true

                text: qsTrc("effects/normalize", "Normalize peak amplitude to")
                onClicked: {
                    normalize.normalizePeakAmplitude = !checked
                }
                checked: normalize.normalizePeakAmplitude
            }

            IncrementalPropertyControl {
                Layout.alignment: Qt.AlignVCenter | Qt.AlignRight
                Layout.preferredWidth: 100

                measureUnitsSymbol: qsTrc("global", "dB")
                decimals: 2
                step: 0.1
                maxValue: 0
                enabled: normalizePeakAmplitudeCheckbox.checked
                currentValue: normalize.peakAmplitudeTarget
                onValueEdited: function (newValue) {
                    if (newValue !== normalize.peakAmplitudeTarget) {
                        normalize.peakAmplitudeTarget = newValue
                    }
                }
            }
        }

        CheckBox {
            id: normalizeStereoChannelsIndependentlyCheckbox
            text: qsTrc("effects/normalize", "Normalize stereo channels independently")

            onClicked: {
                normalize.normalizeStereoChannelsIndependently = !checked
            }
            enabled: normalize.normalizePeakAmplitude
            checked: normalize.normalizeStereoChannelsIndependently
        }

        Item {
            id: spacer
            height: 16 - column.spacing
            width: 1
        }
    }
}
