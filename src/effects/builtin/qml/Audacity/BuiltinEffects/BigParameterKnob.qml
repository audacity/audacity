import QtQuick
import QtQuick.Layouts
import Audacity.ProjectScene
import Audacity.BuiltinEffects
import Muse.UiComponents

Item {
    id: root

    required property var parameter

    property alias stepSize: knob.stepSize
    property alias radius: knob.radius
    property alias warpingType: warper.warpingType
    property bool knobFirst: true

    implicitWidth: content.implicitWidth
    implicitHeight: content.implicitHeight

    signal newValueRequested(string key, real newValue)
    signal commitRequested

    onParameterChanged: {
        if (parameter) {
            knob.from = parameter["min"]
            knob.to = parameter["max"]
            warper.value = parameter["value"]
            knob.stepSize = parameter["step"] || 1
            textEdit.measureUnitsSymbol = parameter["unit"] || ""
        }
    }

    Component.onCompleted: {
        warper.init()
    }

    ValueWarper {
        id: warper

        min: knob.from
        max: knob.to

        onValueChanged: {
            root.newValueRequested(root.parameter["key"], warper.value)
        }
    }

    Column {
        id: content

        spacing: 6

        StyledTextLabel {
            anchors.horizontalCenter: parent.horizontalCenter

            visible: !root.knobFirst
            text: parameter["title"]
            height: 16
            horizontalAlignment: Qt.AlignHCenter
        }

        KnobControl {
            id: knob

            value: warper.warpedValue

            anchors.horizontalCenter: parent.horizontalCenter

            onNewValueRequested: function (value) {
                warper.warpedValue = value
            }

            mouseArea.onReleased: function () {
                root.commitRequested()
            }
        }

        StyledTextLabel {
            anchors.horizontalCenter: parent.horizontalCenter

            visible: root.knobFirst
            text: parameter["title"]
            height: 16
            horizontalAlignment: Qt.AlignHCenter
        }

        IncrementalPropertyControl {
            id: textEdit

            anchors.horizontalCenter: parent.horizontalCenter

            implicitWidth: 70

            minValue: knob.from
            maxValue: knob.to
            decimals: {
                let s = knob.stepSize.toString()
                if (s.indexOf('.') >= 0)
                    return s.split('.')[1].length
                return 0
            }
            step: knob.stepSize

            currentValue: +warper.value.toFixed(decimals)

            onValueEdited: function (value) {
                root.newValueRequested(root.parameter["key"], value)
            }

            onValueEditingFinished: function (value) {
                root.commitRequested()
            }
        }
    }
}
