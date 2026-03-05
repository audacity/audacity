import QtQuick 2.15
import Audacity.BuiltinEffects
import Audacity.BuiltinEffectsCollection

Item {
    id: root

    required property var model
    property var navigationPanel: null
    property int navigationOrder: 0
    property bool modelInitialized: false
    property bool warp: false
    property bool isVertical: false
    property bool knobFirst: true // Only relevant is `isVertical` is true
    property int radius: 16

    function initModelIfNeeded() {
        if (!root.model) {
            return
        }

        if (root.modelInitialized) {
            return
        }

        root.model.init()
        root.modelInitialized = true
    }

    onModelChanged: {
        root.modelInitialized = false
        initModelIfNeeded()
    }

    Component.onCompleted: {
        initModelIfNeeded()
    }

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
            navigation.panel: root.navigationPanel
            navigation.order: root.navigationOrder
            radius: root.radius
            defaultValue: root.model.defaultValue
            middle: root.warp ? root.model.defaultValue : null
            knobFirst: root.knobFirst
            parameter: {
                "title": root.model.title,
                "unit": root.model.unit,
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
            navigation.panel: root.navigationPanel
            navigation.order: root.navigationOrder
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
