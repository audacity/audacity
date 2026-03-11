import QtQuick
import QtQuick.Layouts
import Audacity.ProjectScene
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection
import Muse.UiComponents

Item {
    id: root

    required property var parameter

    property alias navigation: knob.navigation

    property double defaultValue: 0
    property alias stepSize: knob.stepSize
    property alias radius: knob.radius
    property alias middle: warper.middle
    property bool knobFirst: true

    implicitWidth: content.implicitWidth
    implicitHeight: content.implicitHeight

    signal newValueRequested(string key, real newValue)
    signal commitRequested

    function effectiveStepSize() {
        const visibleStep = (knob.to - knob.from) / 50
        const parameterStep = Number(parameter["step"])

        if (isFinite(parameterStep) && parameterStep > 0) {
            return Math.max(visibleStep, parameterStep)
        }

        return visibleStep
    }

    function precisionStepSize() {
        const parameterStep = Number(parameter["step"])

        if (isFinite(parameterStep) && parameterStep > 0) {
            return parameterStep
        }

        return knob.stepSize
    }

    function normalizedValue(value) {
        return Number(value.toFixed(textEdit.decimals))
    }

    function activateNumericInput(initialText) {
        if (!textEdit.activeFocus) {
            textEdit.forceActiveFocus()
        }
        if (initialText !== undefined && initialText !== "") {
            textEdit.currentText = initialText
        }
    }

    onParameterChanged: {
        if (parameter) {
            knob.from = parameter["min"]
            knob.to = parameter["max"]
            warper.value = parameter["value"]
            knob.stepSize = effectiveStepSize()
            textEdit.measureUnitsSymbol = parameter["unit"] || ""
        }
    }

    ValueWarper {
        id: warper

        min: knob.from
        max: knob.to

        onValueChanged: {
            root.newValueRequested(root.parameter["key"], root.normalizedValue(warper.value))
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

            mouseArea.onDoubleClicked: function () {
                root.newValueRequested(root.parameter["key"], root.defaultValue)
            }

            mouseArea.onReleased: function () {
                root.commitRequested()
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
                let s = root.precisionStepSize().toString()
                if (s.indexOf('.') >= 0)
                    return s.split('.')[1].length
                return 0
            }
            step: root.precisionStepSize()

            currentValue: +warper.value.toFixed(decimals)

            onValueEdited: function (value) {
                root.newValueRequested(root.parameter["key"], root.normalizedValue(value))
            }

            onValueEditingFinished: function (value) {
                root.commitRequested()
            }
        }
    }
}
