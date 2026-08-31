import QtQuick
import QtQuick.Layouts
import Audacity.ProjectScene
import Muse.UiComponents

Item {
    id: root

    required property var parameter

    property alias navigation: knob.navigation

    property alias value: knob.value
    property double defaultValue: 0
    property alias radius: knob.radius

    implicitWidth: content.implicitWidth
    implicitHeight: content.implicitHeight

    signal newValueRequested(string key, real newValue)
    signal commitRequested

    function activateNumericInput(initialText) {
        if (!textEdit.activeFocus) {
            textEdit.forceActiveFocus()
        }
        if (initialText !== undefined && initialText !== "") {
            textEdit.currentText = initialText
        }
    }

    // Fractional digits needed to display the step (numeric, locale-independent)
    function decimalsForStep(step) {
        let d = 0
        let s = step
        while (d < 6 && Math.abs(s - Math.round(s)) > 1e-9 * Math.max(1, Math.abs(s))) {
            s *= 10
            d++
        }
        return d
    }

    onParameterChanged: {
        if (parameter) {
            knob.from = parameter["min"]
            knob.to = parameter["max"]
            knob.value = parameter["value"]
            knob.stepSize = parameter["step"] || 1
            textEdit.measureUnitsSymbol = parameter["unit"] || ""
        }
    }

    Column {
        id: content

        spacing: 6

        StyledTextLabel {
            text: parameter["title"]
            height: 16
            horizontalAlignment: Qt.AlignLeft
        }

        Row {
            spacing: 8

            KnobControl {
                id: knob

                navigation.accessible.name: root.parameter["title"]

                onNewValueRequested: function (value) {
                    root.newValueRequested(root.parameter["key"], value)
                }

                mouseArea.onReleased: function () {
                    root.commitRequested()
                }

                mouseArea.onDoubleClicked: function () {
                    root.newValueRequested(root.parameter["key"], root.defaultValue)
                }
            }

            Connections {
                target: knob.navigation

                function onNavigationEvent(event) {
                    if (event.type !== NavigationEvent.Trigger) {
                        return
                    }

                    root.activateNumericInput()
                    event.accepted = true
                }
            }

            IncrementalPropertyControl {
                id: textEdit

                navigation.accessible.name: root.parameter["title"] + " " + currentValue

                width: 80

                minValue: knob.from
                maxValue: knob.to
                decimals: root.decimalsForStep(knob.stepSize)
                step: knob.stepSize

                currentValue: ui.df.roundReal(knob.value, decimals)

                onValueEdited: function (value) {
                    root.newValueRequested(root.parameter["key"], value)
                }

                onValueEditingFinished: function (value) {
                    root.commitRequested()
                }
            }
        }
    }
}
