import QtQuick
import QtQuick.Layouts
import Audacity.ProjectScene
import Muse.UiComponents

Item {
    id: root

    required property var parameter

    property alias value: knob.value
    property alias radius: knob.radius

    implicitWidth: content.implicitWidth
    implicitHeight: content.implicitHeight

    signal newValueRequested(string key, real newValue)
    signal commitRequested

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

                onNewValueRequested: function (value) {
                    root.newValueRequested(root.parameter["key"], value)
                }

                mouseArea.onReleased: function () {
                    root.commitRequested()
                }
            }

            IncrementalPropertyControl {
                id: textEdit

                width: 80

                minValue: knob.from
                maxValue: knob.to
                decimals: {
                    let s = knob.stepSize.toString()
                    if (s.indexOf('.') >= 0)
                        return s.split('.')[1].length
                    return 0
                }
                step: knob.stepSize

                currentValue: +knob.value.toFixed(decimals)

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
