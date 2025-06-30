import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

Item {
    id: root

    property var meterModel: null

    width: 24

    readonly property real topTextMargin: topText.paintedHeight / 2
    readonly property real bottomTextMargin: bottomText.paintedHeight / 2
    readonly property real effectiveHeight: height - topTextMargin - bottomTextMargin

    onEffectiveHeightChanged: {
        root.meterModel.meterSize = root.effectiveHeight;
    }

    // Draw small steps
    Repeater {
        id: smallStepsRepeater

        model: root.meterModel.smallSteps

        delegate: Item {
            required property real modelData

            y: root.topTextMargin + (1.0 - root.meterModel.stepToPosition(modelData)) * root.effectiveHeight
            x: 0
            height: 1
            width: parent.width

            Rectangle {
                width: 4
                height: 1
                color: Utils.colorWithAlpha(ui.theme.fontPrimaryColor, 0.5)
            }
        }
    }

    // Draw bigger steps and numbers
    Repeater {
        id: fullStepsRepeater

        model: root.meterModel.fullSteps

        delegate: Item {
            required property real modelData

            y: root.topTextMargin + (1.0 - root.meterModel.stepToPosition(modelData)) * root.effectiveHeight
            width: parent.width
            height: 1
            
            Row {
                anchors.horizontalCenter: parent.horizontalCenter
                anchors.verticalCenter: parent.verticalCenter
                spacing: 2

                // Draw the full step tick
                Rectangle {
                    width: 4
                    height: 1
                    color: ui.theme.fontPrimaryColor
                    anchors.verticalCenter: parent.verticalCenter

                    visible: modelData < 0
                }

                // Draw the label with the value
                Text {
                    id: aLabel
                    text: root.meterModel.sampleToText(modelData)
                    color: ui.theme.fontPrimaryColor
                    font.pixelSize: 10
                }
            }
        }
    }
    
    Text { id: topText; text: "0"; font.pixelSize: 10; visible: false }
    Text { id: bottomText; text: "-60"; font.pixelSize: 10; visible: false }
}
