
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

Item {
    id: root

    property var meterModel: null

    height: rightText.paintedHeight

    readonly property real leftTextMargin: leftText.paintedWidth / 2
    readonly property real rightTextMargin: rightText.paintedWidth / 2
    readonly property real effectiveWidth: width - leftTextMargin - rightTextMargin

    onEffectiveWidthChanged: {
        root.meterModel.meterSize = root.effectiveWidth;
    }

    // Draw bigger steps and numbers
    Repeater {
        id: fullStepsRepeater

        model: root.meterModel.fullSteps

        delegate: Item {
            required property real modelData

            x: root.leftTextMargin + root.meterModel.stepToPosition(modelData) * root.effectiveWidth
            height: parent.height
            width: 1

            Row {
                anchors.horizontalCenter: parent.horizontalCenter
                anchors.verticalCenter: parent.verticalCenter
                spacing: modelData < 0 ? 2 : 0

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
    
    Text { id: leftText; text: "0.00"; font.pixelSize: 10; visible: false }
    Text { id: rightText; text: "1.00"; font.pixelSize: 10; visible: false }
}