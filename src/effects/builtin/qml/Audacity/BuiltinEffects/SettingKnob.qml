import QtQuick 2.15
import Audacity.BuiltinEffects

Item {
    id: root

    required property var model
    required property string title
    property string unit: ""
    property bool warp: false
    property bool isVertical: false
    property bool knobFirst: true // Only relevant is `isVertical` is true
    property int radius: 16

    Loader {
        id: knobLoader

        onLoaded: {
            root.width = knobLoader.item.width
            root.height = knobLoader.item.height
        }
        sourceComponent: isVertical ? verticalKnob : horizontalKnob
    }

    Component {
        id: verticalKnob

        BigParameterKnob {
            radius: root.radius
            defaultValue: root.model.defaultValue
            middle: root.warp ? root.model.defaultValue : null
            knobFirst: root.knobFirst
            parameter: {
                "title": root.title,
                "unit": root.unit,
                "min": root.model.min,
                "max": root.model.max,
                "value": root.model.value,
                "step": root.model.step
            }

            onNewValueRequested: function (_, newValue) {
                root.model.value = newValue
            }

            onCommitRequested: {
                root.model.commitSettings()
            }
        }
    }

    Component {
        id: horizontalKnob

        ParameterKnob {
            radius: root.radius
            defaultValue: root.model.defaultValue
            parameter: {
                "title": root.title,
                "unit": root.unit,
                "min": root.model.min,
                "max": root.model.max,
                "value": root.model.value,
                "step": root.model.step
            }

            onNewValueRequested: function (_, newValue) {
                root.model.value = newValue
            }

            onCommitRequested: {
                root.model.commitSettings()
            }
        }
    }
}
