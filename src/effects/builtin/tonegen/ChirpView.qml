import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import Muse.Ui
import Muse.UiComponents
import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.Playback

// TODO: move to common controls
import Preferences

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects", "Chirp")
    property alias isApplyAllowed: chirp.isApplyAllowed

    width: 370
    implicitHeight: column.height

    model: chirp

    QtObject {
        id: prv

        readonly property int padding: ui.theme.extra.space_32
        readonly property int interpolationLinear: 0
        readonly property int interpolationLogarithmic: 1
    }

    ToneViewModel {
        id: chirp
    }

    ColumnLayout {
        id: column

        width: parent.width

        spacing: ui.theme.extra.space_16

        ComboBoxWithTitle {

            title: qsTrc("effects/chirp", "Waveform")
            columnWidth: parent.width

            control.background.color: ui.theme.backgroundPrimaryColor
            control.background.border.width: 1
            control.itemColor: "transparent"

            currentIndex: chirp.waveform
            model: chirp.waveforms

            onValueEdited: function (newIndex, newValue) {
                chirp.waveform = newIndex
            }
        }

        RoundedRectangle {

            Layout.fillWidth: true
            Layout.preferredHeight: frequencyGroup.height + (ui.theme.extra.space_16 * 2)

            color: ui.theme.backgroundSecondaryColor

            border.color: ui.theme.strokeColor
            border.width: 1

            radius: 4

            Column {
                id: frequencyGroup

                width: parent.width - (ui.theme.extra.space_16 * 2)
                x: ui.theme.extra.space_16
                y: ui.theme.extra.space_16

                spacing: ui.theme.extra.space_16

                Column {

                    spacing: ui.theme.extra.space_8

                    StyledTextLabel {
                        text: qsTrc("effects/chirp", "Frequency sweep")
                    }

                    Row {

                        spacing: ui.theme.extra.space_16

                        RoundedRadioButton {

                            text: qsTrc("effects/chirp", "Linear")
                            checked: chirp.interpolation == prv.interpolationLinear

                            onToggled: {
                                if (chirp.interpolation != prv.interpolationLinear) {
                                    chirp.interpolation = prv.interpolationLinear
                                }
                            }
                        }

                        RoundedRadioButton {

                            text: qsTrc("effects/chirp", "Logarithmic")
                            checked: chirp.interpolation == prv.interpolationLogarithmic

                            onToggled: {
                                if (chirp.interpolation != prv.interpolationLogarithmic) {
                                    chirp.interpolation = prv.interpolationLogarithmic
                                }
                            }
                        }
                    }
                }

                Row {

                    spacing: ui.theme.extra.space_16
                    width: parent.width

                    IncrementalPropertyControlWithTitle {

                        columnWidth: (parent.width - parent.spacing) / 2
                        controlWidth: (parent.width - parent.spacing) / 2

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

                        columnWidth: (parent.width - parent.spacing) / 2
                        controlWidth: (parent.width - parent.spacing) / 2

                        title: qsTrc("effects/chirp", "End frequency")
                        currentValue: chirp.frequencyEnd

                        minValue: 1
                        maxValue: 1000000

                        measureUnitsSymbol: qsTrc("global", "Hz")

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

                spacing: ui.theme.extra.space_16
                width: parent.width - prv.padding
                height: implicitHeight

                x: ui.theme.extra.space_16
                y: ui.theme.extra.space_16

                IncrementalPropertyControlWithTitle {

                    columnWidth: (parent.width - parent.spacing) / 2
                    controlWidth: parent.width * .25

                    title: qsTrc("effects/chirp", "Start amplitude (0-1)")
                    currentValue: chirp.amplitudeStart

                    minValue: 0
                    maxValue: 1
                    decimals: 4
                    step: 0.01

                    onValueEdited: function (newValue) {
                        if (chirp.amplitudeStart !== newValue) {
                            chirp.amplitudeStart = newValue
                        }
                    }
                }

                IncrementalPropertyControlWithTitle {

                    columnWidth: (parent.width - parent.spacing) / 2
                    controlWidth: parent.width * .25

                    title: qsTrc("effects/chirp", "End amplitude (0-1)")
                    currentValue: chirp.amplitudeEnd

                    minValue: 0
                    maxValue: 1
                    decimals: 4
                    step: 0.01

                    onValueEdited: function (newValue) {
                        if (chirp.amplitudeEnd !== newValue) {
                            chirp.amplitudeEnd = newValue
                        }
                    }
                }
            }
        }

        Column {

            spacing: ui.theme.extra.space_8

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

                onValueChanged: {
                    chirp.duration = timecode.value
                }
            }
        }

        Item {
            Layout.fillHeight: true
        }
    }
}
