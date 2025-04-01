import QtQuick
import QtQuick.Layouts
import Audacity.ProjectScene
import Muse.UiComponents

Item {
    id: root

    required property var parameter

    property alias value: knob.value

    implicitWidth: content.implicitWidth
    implicitHeight: content.implicitHeight

    signal newValueRequested(string key, real newValue)

    onParameterChanged: {
        if (parameter) {
            knob.from = parameter["min"]
            knob.to = parameter["max"]
            knob.value = parameter["value"]
            textEdit.measureUnitsSymbol = parameter["unit"]
        }
    }

    Column {
        id: content

        spacing: 8

        StyledTextLabel {
            text:  parameter["title"]
            horizontalAlignment: Qt.AlignLeft
        }

        Row {
            spacing: 8

            KnobControl {
                id: knob

                stepSize: 1

                onNewValueRequested: function (value) {
                    root.newValueRequested(root.parameter["key"], value)
                }
            }

            IncrementalPropertyControl {
                width: 80

                minValue: knob.from
                maxValue: knob.to
                decimals: 0
                step: knob.stepSize

                currentValue: +knob.value.toFixed(decimals)

                onValueEdited: function(value) {
                    root.newValueRequested(root.parameter["key"], value)
                }
            }
        }
    }
}
