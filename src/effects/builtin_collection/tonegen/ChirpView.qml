import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Muse.Ui
import Muse.UiComponents
import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection
import Audacity.UiComponents

// TODO: move to common controls
import Audacity.Preferences

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects", "Chirp")
    property bool isApplyAllowed: chirp.isApplyAllowed

    width: 370
    implicitHeight: column.height

    builtinEffectModel: ToneViewModelFactory.createModel(root, root.instanceId)
    numNavigationPanels: 5
    property alias chirp: root.builtinEffectModel
    property NavigationPanel waveformNavigationPanel: NavigationPanel {
        name: "ChirpWaveform"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }
    property NavigationPanel frequencyNavigationPanel: NavigationPanel {
        name: "ChirpFrequencyGroup"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 2
    }
    property NavigationPanel amplitudeNavigationPanel: NavigationPanel {
        name: "ChirpAmplitudeGroup"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 3
    }
    property NavigationPanel durationNavigationPanel: NavigationPanel {
        name: "ChirpDuration"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 4
    }

    QtObject {
        id: prv

        readonly property int spacing: 16
        readonly property int padding: 32
        readonly property int interpolationLinear: 0
        readonly property int interpolationLogarithmic: 1
    }

    ColumnLayout {
        id: column

        width: parent.width

        spacing: prv.spacing

        ComboBoxWithTitle {
            id: waveformDropdown

            title: qsTrc("effects/chirp", "Waveform")
            columnWidth: parent.width

            control.background.color: ui.theme.backgroundPrimaryColor
            control.background.border.width: 1
            control.itemColor: "transparent"

            currentIndex: chirp.waveform
            model: chirp.waveforms

            navigation.panel: root.waveformNavigationPanel
            navigation.order: 0

            onValueEdited: function (newIndex, newValue) {
                chirp.waveform = newIndex
            }
        }

        RoundedRectangle {

            Layout.fillWidth: true
            Layout.preferredHeight: frequencyGroup.height + prv.spacing * 2

            color: ui.theme.backgroundSecondaryColor

            border.color: ui.theme.strokeColor
            border.width: 1

            radius: 4

            Column {
                id: frequencyGroup

                width: parent.width - prv.spacing * 2
                x: prv.spacing
                y: prv.spacing

                spacing: prv.spacing

                Column {

                    spacing: 8

                    StyledTextLabel {
                        text: qsTrc("effects/chirp", "Frequency sweep")
                    }

                    Row {

                        spacing: prv.spacing

                        RoundedRadioButton {
                            id: interpolationLinearRadio

                            text: qsTrc("effects/chirp", "Linear")
                            checked: chirp.interpolation == prv.interpolationLinear

                            navigation.panel: root.frequencyNavigationPanel
                            navigation.order: 0

                            onToggled: {
                                if (chirp.interpolation != prv.interpolationLinear) {
                                    chirp.interpolation = prv.interpolationLinear
                                }
                            }
                        }

                        RoundedRadioButton {
                            id: interpolationLogarithmicRadio

                            text: qsTrc("effects/chirp", "Logarithmic")
                            checked: chirp.interpolation == prv.interpolationLogarithmic

                            navigation.panel: root.frequencyNavigationPanel
                            navigation.order: interpolationLinearRadio.navigation.order + 1

                            onToggled: {
                                if (chirp.interpolation != prv.interpolationLogarithmic) {
                                    chirp.interpolation = prv.interpolationLogarithmic
                                }
                            }
                        }
                    }
                }

                Row {

                    spacing: prv.spacing
                    width: parent.width

                    IncrementalPropertyControlWithTitle {
                        id: startFrequencyControl

                        columnWidth: (parent.width - parent.spacing) / 2
                        controlWidth: (parent.width - parent.spacing) / 2

                        navigation.panel: root.frequencyNavigationPanel
                        navigation.order: interpolationLogarithmicRadio.navigation.order + 1

                        title: qsTrc("effects/chirp", "Start frequency")
                        currentValue: chirp.frequencyStart

                        minValue: 1
                        maxValue: 1000000

                        measureUnitsSymbol: qsTrc("global", "Hz")

                        onValueEdited: function (newValue) {
                            if (chirp.frequencyStart !== newValue) {
                                chirp.frequencyStart = newValue
                            }
                        }
                    }

                    IncrementalPropertyControlWithTitle {
                        id: endFrequencyControl

                        columnWidth: (parent.width - parent.spacing) / 2
                        controlWidth: (parent.width - parent.spacing) / 2

                        title: qsTrc("effects/chirp", "End frequency")
                        currentValue: chirp.frequencyEnd

                        minValue: 1
                        maxValue: 1000000

                        measureUnitsSymbol: qsTrc("global", "Hz")

                        navigation.panel: root.frequencyNavigationPanel
                        navigation.order: startFrequencyControl.navigation.order + 1

                        onValueEdited: function (newValue) {
                            if (chirp.frequencyEnd !== newValue) {
                                chirp.frequencyEnd = newValue
                            }
                        }
                    }
                }
            }
        }

        RoundedRectangle {

            Layout.fillWidth: true
            Layout.preferredHeight: amplitudeGroup.height + prv.padding

            color: ui.theme.backgroundSecondaryColor

            border.color: ui.theme.strokeColor
            border.width: 1

            radius: 4

            Row {
                id: amplitudeGroup

                spacing: prv.spacing
                width: parent.width - prv.padding
                height: implicitHeight

                x: prv.spacing
                y: prv.spacing

                IncrementalPropertyControlWithTitle {
                    id: startAmplitudeControl

                    columnWidth: (parent.width - parent.spacing) / 2
                    controlWidth: parent.width * .25

                    title: qsTrc("effects/chirp", "Start amplitude (0-1)")
                    currentValue: chirp.amplitudeStart

                    minValue: 0
                    maxValue: 1
                    decimals: 4
                    step: 0.01

                    navigation.panel: root.amplitudeNavigationPanel
                    navigation.order: 0

                    onValueEdited: function (newValue) {
                        if (chirp.amplitudeStart !== newValue) {
                            chirp.amplitudeStart = newValue
                        }
                    }
                }

                IncrementalPropertyControlWithTitle {
                    id: endAmplitudeControl

                    columnWidth: (parent.width - parent.spacing) / 2
                    controlWidth: parent.width * .25

                    title: qsTrc("effects/chirp", "End amplitude (0-1)")
                    currentValue: chirp.amplitudeEnd

                    minValue: 0
                    maxValue: 1
                    decimals: 4
                    step: 0.01

                    navigation.panel: root.amplitudeNavigationPanel
                    navigation.order: startAmplitudeControl.navigation.order + 1

                    onValueEdited: function (newValue) {
                        if (chirp.amplitudeEnd !== newValue) {
                            chirp.amplitudeEnd = newValue
                        }
                    }
                }
            }
        }

        Column {

            spacing: 8

            StyledTextLabel {
                text: qsTrc("effects/chirp", "Duration")
            }

            Timecode {
                id: timecode

                Layout.fillHeight: false
                Layout.columnSpan: 2

                border: Border {
                    color: ui.theme.strokeColor
                    width: 1
                }

                arrowSpacing: -2
                backgroundColor: ui.theme.backgroundSecondaryColor
                textColor: ui.theme.fontPrimaryColor

                value: chirp.duration
                mode: TimecodeModeSelector.Duration
                currentFormatStr: chirp.durationFormat
                sampleRate: chirp.sampleRate
                tempo: chirp.tempo
                upperTimeSignature: chirp.upperTimeSignature
                lowerTimeSignature: chirp.lowerTimeSignature
                enabled: true

                navigation.panel: root.durationNavigationPanel
                navigation.order: 0

                onValueChanged: {
                    chirp.duration = timecode.value
                }
            }
        }
    }
}
