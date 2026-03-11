import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.UiComponents

import Audacity.UiComponents
import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

BuiltinEffectBase {
    id: root

    property string title: qsTrc("projectscene/silence", "Silence")
    property bool isApplyAllowed: silence.isApplyAllowed

    implicitWidth: 300
    implicitHeight: column.implicitHeight

    builtinEffectModel: SilenceViewModelFactory.createModel(root, root.instanceId)
    property alias silence: root.builtinEffectModel

    numNavigationPanels: 2
    property NavigationPanel timecodeNavigationPanel: NavigationPanel {
        name: "SilenceDuration"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }

    Column {
        id: column

        anchors.fill: parent

        spacing: 16

        StyledTextLabel {
            text: qsTrc("projectscene/silence", "Duration:")
        }

        Timecode {
            id: timecode

            value: silence.duration
            mode: TimecodeModeSelector.Duration
            currentFormatStr: silence.durationFormat
            sampleRate: silence.sampleRate
            tempo: silence.tempo
            upperTimeSignature: silence.upperTimeSignature
            lowerTimeSignature: silence.lowerTimeSignature

            navigation.panel: root.timecodeNavigationPanel
            navigation.order: 0

            onValueChanged: {
                silence.duration = timecode.value
            }

            onCurrentFormatChanged: {
                silence.durationFormat = timecode.currentFormatStr
            }
        }
    }
}
