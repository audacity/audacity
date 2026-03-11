import QtQuick
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects/normalize", "Normalize")
    property bool isApplyAllowed: removeDcCheckbox.checked || normalizePeakAmplitudeCheckbox.checked

    width: 400
    implicitHeight: column.height

    builtinEffectModel: NormalizeViewModelFactory.createModel(root, root.instanceId)
    numNavigationPanels: 1
    property alias normalize: root.builtinEffectModel
    property NavigationPanel normalizeNavigationPanel: NavigationPanel {
        name: "NormalizeControls"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }

    Column {
        id: column

        topPadding: 2
        bottomPadding: 2
        spacing: 10

        CheckBox {
            id: removeDcCheckbox

            navigation.panel: root.normalizeNavigationPanel
            navigation.order: 0

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

                navigation.panel: root.normalizeNavigationPanel
                navigation.order: removeDcCheckbox.navigation.order + 1

                text: qsTrc("effects/normalize", "Normalize peak amplitude to")
                onClicked: {
                    normalize.normalizePeakAmplitude = !checked
                }
                checked: normalize.normalizePeakAmplitude
            }

            IncrementalPropertyControl {
                id: peakAmplitudeControl

                Layout.alignment: Qt.AlignVCenter | Qt.AlignRight
                Layout.preferredWidth: 100

                navigation.panel: root.normalizeNavigationPanel
                navigation.order: normalizePeakAmplitudeCheckbox.navigation.order + 1

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

            navigation.panel: root.normalizeNavigationPanel
            navigation.order: peakAmplitudeControl.navigation.order + 1

            text: qsTrc("effects/normalize", "Normalize stereo channels independently")

            onClicked: {
                normalize.normalizeStereoChannelsIndependently = !checked
            }
            enabled: normalize.normalizePeakAmplitude
            checked: normalize.normalizeStereoChannelsIndependently
        }
    }
}
