import QtQuick 2.15

import Muse.UiComponents

import Audacity.Effects

import "../common"

EffectBase {

    id: root

    property string title: qsTrc("effects", "Reverb")
    property bool isApplyAllowed: true

    height: 400
    width: 400

    model: reverb

    ReverbViewModel {
        id: reverb
        instanceId: root.instanceId
    }

    Component.onCompleted: {
        reverb.init()
    }

    component ParamItem: Item {
        id: param
        property alias title: label.text
        property real value: 0.0
        property alias min: slider.from
        property alias max: slider.to

        height: 40
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.margins: 8

        StyledTextLabel {
            id: label
            anchors.left: parent.left
            anchors.top: parent.top
            anchors.bottom: parent.bottom
            verticalAlignment: Text.AlignVCenter
            horizontalAlignment: Text.AlignLeft
            width: 120
        }

        IncrementalPropertyControl {
            id: numField
            anchors.left: label.right
            anchors.verticalCenter: parent.verticalCenter
            width: 68
            maxValue: param.max
            minValue: param.min
            step: 1.0
            currentValue: param.value.toFixed(1) // same as slider step
            onValueEdited: function(newVal) {
                param.value = newVal
            }
        }

        StyledSlider {
            id: slider
            anchors.right: parent.right
            anchors.left: numField.right
            anchors.leftMargin: 8
            anchors.verticalCenter: parent.verticalCenter
            stepSize: 0.1
            value: param.value
            onMoved: param.value = slider.value
        }
    }

    Column {
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: wetOnly.top

        Repeater {
            id: repeator
            model: reverb.paramsList
            delegate: ParamItem {
                title: modelData.title
                value: modelData.value
                min: modelData.min
                max: modelData.max

                onValueChanged: reverb.setParam(modelData.key, value)
            }
        }
    }

    CheckBox {
        id: wetOnly

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom

        text: qsTrc("effects", "Wet Only")
        checked: reverb.wetOnly
        onClicked: reverb.wetOnly = !reverb.wetOnly
    }
}
