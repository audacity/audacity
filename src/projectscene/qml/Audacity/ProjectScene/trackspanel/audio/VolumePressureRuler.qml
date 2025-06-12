import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

Item {
    id: root

    property real minVolumePressure: -60.0
    property real maxVolumePressure: 0.0

    property int minimalFullStepSize: 20 

    width: 24

    readonly property real topTextMargin: topText.paintedHeight / 2
    readonly property real bottomTextMargin: bottomText.paintedHeight / 2
    readonly property real effectiveHeight: height - topTextMargin - bottomTextMargin

    QtObject {
        id: prv

        readonly property real valueRange: root.maxVolumePressure - root.minVolumePressure

        readonly property int fullStepValue: 6
        readonly property int smallStepValue: 2
        
        readonly property int fullStepCount: Math.floor(prv.valueRange / prv.fullStepValue)
        readonly property int smallStepCount: Math.floor(prv.valueRange / prv.smallStepValue)
    }

    // Draw small steps
    Repeater {
        model: prv.smallStepCount

        delegate: Item {
            readonly property real value: root.maxVolumePressure - (index * prv.smallStepValue)
            
            // Skip drawing if this is a full step's position
            visible: value % prv.fullStepValue !== 0

            readonly property real ratio: (value - root.minVolumePressure) / prv.valueRange
            y: root.topTextMargin + (1.0 - ratio) * root.effectiveHeight
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
        model: prv.fullStepCount + 1

        delegate: Item {
            readonly property int value: root.maxVolumePressure - (index * prv.fullStepValue)
            readonly property real ratio: (value - root.minVolumePressure) / prv.valueRange
            y: root.topTextMargin + (1.0 - ratio) * root.effectiveHeight
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

                    visible: value != 0
                }

                // Draw the label with the value
                Text {
                    id: aLabel
                    text: Math.abs(value)
                    color: ui.theme.fontPrimaryColor
                    font.pixelSize: 10
                }
            }
        }
    }
    
    Text { id: topText; text: root.minVolumePressure; font.pixelSize: 10; visible: false }
    Text { id: bottomText; text: root.maxVolumePressure; font.pixelSize: 10; visible: false }
}


        // function roundUpToFixedValue(value) {
        //     // full and small step is
        //     // a number units per each respective notch on the ruler
        //     const steps = [
        //         { fullStep: 1, smallStep: 0 },
        //         { fullStep: 2, smallStep: 1 },
        //         { fullStep: 6, smallStep: 2 },
        //         { fullStep: 10, smallStep: 2 },
        //         { fullStep: 20, smallStep: 4 },
        //         { fullStep: 50, smallStep: 10 },
        //         { fullStep: 100, smallStep: 20 }
        //     ];

        //     // Find the nearest full step
        //     for (let i = 0; i < steps.length; i++) {
        //         if (value <= steps[i].fullStep) {
        //             return steps[i];
        //         }
        //     }

        //     // Should not happen
        //     return steps[steps.length - 1];
        // }