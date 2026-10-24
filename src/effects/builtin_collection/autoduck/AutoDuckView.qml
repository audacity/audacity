import QtQuick
import QtQuick.Layouts
import Muse.Ui
import Muse.UiComponents
import Muse.GraphicalEffects
import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects/autoduck", "Auto duck")
    property bool isApplyAllowed: autoDuck.hasControlTrack

    implicitWidth: prv.contentWidth
    implicitHeight: column.implicitHeight

    builtinEffectModel: AutoDuckViewModelFactory.createModel(root, root.instanceId)
    numNavigationPanels: 4
    property alias autoDuck: root.builtinEffectModel

    property NavigationPanel controlTrackNavigationPanel: NavigationPanel {
        name: "AutoDuckControlTrack"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }

    property NavigationPanel thresholdNavigationPanel: NavigationPanel {
        name: "AutoDuckThreshold"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 2
    }

    property NavigationPanel duckPanelNavigationPanel: NavigationPanel {
        name: "AutoDuckDuckAndRecovery"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 3
    }

    property NavigationPanel holdNavigationPanel: NavigationPanel {
        name: "AutoDuckHold"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 4
    }

    QtObject {
        id: prv

        readonly property int contentWidth: 608
        readonly property int spacing: 16

        // Threshold and hold knobs on either side of the duck-and-recovery panel
        readonly property int sideColumnWidth: 88
        readonly property int sideKnobRadius: 24

        readonly property int panelHeight: 220
        readonly property int panelPadding: 16
        readonly property int panelKnobRadius: 15
        readonly property int panelWidth: contentWidth - 2 * (sideColumnWidth + spacing)
        readonly property real panelCellWidth: (panelWidth - 2 * panelPadding - 2 * spacing) / 3
        readonly property real panelCellHeight: (panelHeight - 2 * panelPadding - spacing) / 2

        readonly property real timeStep: 0.01
        readonly property real dbStep: 0.1

        //: Abbreviation of "seconds", used as a unit suffix
        readonly property string secondsUnit: qsTrc("global", "s")
        //: Abbreviation of "decibels", used as a unit suffix
        readonly property string dbUnit: qsTrc("global", "dB")
    }

    Column {
        id: column

        width: prv.contentWidth
        spacing: 12

        AutoDuckGraph {
            id: graph

            width: parent.width
            height: graph.width / graph.preferredAspectRatio

            model: autoDuck
            enabled: autoDuck.hasControlTrack
        }

        RowLayout {
            width: parent.width
            height: 28
            spacing: prv.spacing

            StyledTextLabel {
                text: qsTrc("effects/autoduck", "Control track")
            }

            StyledDropdown {
                Layout.preferredWidth: 518
                height: 28

                navigation.panel: root.controlTrackNavigationPanel
                navigation.order: 0
                navigation.accessible.name: qsTrc("effects/autoduck", "Control track")

                enabled: autoDuck.hasControlTrack
                model: autoDuck.controlTrackOptions
                currentIndex: autoDuck.controlTrackIndex
                displayText: autoDuck.hasControlTrack ? currentText : qsTrc("effects/autoduck", "No available track")

                onActivated: function (index, value) {
                    autoDuck.controlTrackIndex = index
                }
            }
        }

        Item {
            width: parent.width
            height: prv.panelHeight

            visible: autoDuck.hasControlTrack

            BigParameterKnob {
                id: thresholdKnob

                x: 0
                y: 12
                width: prv.sideColumnWidth

                navigation.panel: root.thresholdNavigationPanel
                navigation.order: 0

                radius: prv.sideKnobRadius
                defaultValue: autoDuck.defaults["threshold"]
                parameter: {
                    "key": "threshold",
                    "title": qsTrc("effects/autoduck", "Threshold"),
                    "unit": prv.dbUnit,
                    "min": autoDuck.thresholdMin,
                    "max": autoDuck.thresholdMax,
                    "value": autoDuck.threshold,
                    "step": prv.dbStep
                }

                onNewValueRequested: function (key, newValue) {
                    autoDuck.threshold = newValue
                }
            }

            RoundedRectangle {
                id: panel

                x: prv.sideColumnWidth + prv.spacing
                width: prv.panelWidth
                height: parent.height

                color: ui.theme.backgroundSecondaryColor
                border.color: ui.theme.strokeColor
                border.width: 1
                radius: 4

                GridLayout {
                    anchors.fill: parent
                    anchors.margins: prv.panelPadding

                    columns: 3
                    rows: 2
                    columnSpacing: prv.spacing
                    rowSpacing: prv.spacing

                    BigParameterKnob {
                        id: duckStartKnob

                        Layout.preferredWidth: prv.panelCellWidth
                        Layout.preferredHeight: prv.panelCellHeight

                        navigation.panel: root.duckPanelNavigationPanel
                        navigation.order: 0

                        radius: prv.panelKnobRadius
                        defaultValue: autoDuck.defaults["duckStart"]
                        parameter: {
                            "key": "duckStart",
                            "title": qsTrc("effects/autoduck", "Duck start"),
                            "unit": prv.secondsUnit,
                            "min": -autoDuck.fadeLengthMax,
                            "max": 0,
                            "value": autoDuck.duckStart,
                            "step": prv.timeStep
                        }

                        onNewValueRequested: function (key, newValue) {
                            autoDuck.duckStart = newValue
                        }
                    }

                    Image {
                        Layout.preferredWidth: prv.panelCellWidth
                        Layout.preferredHeight: prv.panelCellHeight

                        source: "qrc:/autoduck/duck.png"
                        fillMode: Image.PreserveAspectFit
                        mipmap: true
                        opacity: 0.1

                        layer.enabled: true
                        layer.effect: EffectColorOverlay {
                            color: ui.theme.fontPrimaryColor
                        }
                    }

                    BigParameterKnob {
                        id: recoveryEndKnob

                        Layout.preferredWidth: prv.panelCellWidth
                        Layout.preferredHeight: prv.panelCellHeight

                        navigation.panel: root.duckPanelNavigationPanel
                        navigation.order: recoveryStartKnob.navigation.order + 1

                        radius: prv.panelKnobRadius
                        defaultValue: autoDuck.defaults["recoveryEnd"]
                        parameter: {
                            "key": "recoveryEnd",
                            "title": qsTrc("effects/autoduck", "Recovery end"),
                            "unit": prv.secondsUnit,
                            "min": 0,
                            "max": autoDuck.fadeLengthMax,
                            "value": autoDuck.recoveryEnd,
                            "step": prv.timeStep
                        }

                        onNewValueRequested: function (key, newValue) {
                            autoDuck.recoveryEnd = newValue
                        }
                    }

                    BigParameterKnob {
                        id: duckEndKnob

                        Layout.preferredWidth: prv.panelCellWidth
                        Layout.preferredHeight: prv.panelCellHeight

                        navigation.panel: root.duckPanelNavigationPanel
                        navigation.order: duckStartKnob.navigation.order + 1

                        radius: prv.panelKnobRadius
                        defaultValue: autoDuck.defaults["duckEnd"]
                        parameter: {
                            "key": "duckEnd",
                            "title": qsTrc("effects/autoduck", "Duck end"),
                            "unit": prv.secondsUnit,
                            "min": 0,
                            "max": autoDuck.fadeLengthMax,
                            "value": autoDuck.duckEnd,
                            "step": prv.timeStep
                        }

                        onNewValueRequested: function (key, newValue) {
                            autoDuck.duckEnd = newValue
                        }
                    }

                    BigParameterKnob {
                        id: gainReductionKnob

                        Layout.preferredWidth: prv.panelCellWidth
                        Layout.preferredHeight: prv.panelCellHeight

                        navigation.panel: root.duckPanelNavigationPanel
                        navigation.order: duckEndKnob.navigation.order + 1

                        radius: prv.panelKnobRadius
                        defaultValue: autoDuck.defaults["gainReduction"]
                        parameter: {
                            "key": "gainReduction",
                            "title": qsTrc("effects/autoduck", "Gain reduction"),
                            "unit": prv.dbUnit,
                            "min": autoDuck.gainReductionMin,
                            "max": autoDuck.gainReductionMax,
                            "value": autoDuck.gainReduction,
                            "step": prv.dbStep
                        }

                        onNewValueRequested: function (key, newValue) {
                            autoDuck.gainReduction = newValue
                        }
                    }

                    BigParameterKnob {
                        id: recoveryStartKnob

                        Layout.preferredWidth: prv.panelCellWidth
                        Layout.preferredHeight: prv.panelCellHeight

                        navigation.panel: root.duckPanelNavigationPanel
                        navigation.order: gainReductionKnob.navigation.order + 1

                        radius: prv.panelKnobRadius
                        defaultValue: autoDuck.defaults["recoveryStart"]
                        parameter: {
                            "key": "recoveryStart",
                            "title": qsTrc("effects/autoduck", "Recovery start"),
                            "unit": prv.secondsUnit,
                            "min": -autoDuck.fadeLengthMax,
                            "max": 0,
                            "value": autoDuck.recoveryStart,
                            "step": prv.timeStep
                        }

                        onNewValueRequested: function (key, newValue) {
                            autoDuck.recoveryStart = newValue
                        }
                    }
                }
            }

            BigParameterKnob {
                id: holdKnob

                x: parent.width - width
                y: 12
                width: prv.sideColumnWidth

                navigation.panel: root.holdNavigationPanel
                navigation.order: 0

                radius: prv.sideKnobRadius
                defaultValue: autoDuck.defaults["hold"]
                parameter: {
                    "key": "hold",
                    "title": qsTrc("effects/autoduck", "Hold"),
                    "unit": prv.secondsUnit,
                    "min": 0,
                    "max": autoDuck.holdMax,
                    "value": autoDuck.hold,
                    "step": prv.timeStep
                }

                onNewValueRequested: function (key, newValue) {
                    autoDuck.hold = newValue
                }
            }
        }

        RoundedRectangle {
            width: parent.width
            height: prv.panelHeight

            visible: !autoDuck.hasControlTrack

            color: ui.theme.backgroundSecondaryColor
            border.color: ui.theme.strokeColor
            border.width: 1
            radius: 4

            StyledTextLabel {
                anchors.centerIn: parent
                width: 340

                wrapMode: Text.WordWrap
                horizontalAlignment: Text.AlignHCenter
                text: qsTrc("effects/autoduck", "Auto duck needs a second audio track to trigger the ducking. Add one to your project to continue.")
            }
        }
    }
}
