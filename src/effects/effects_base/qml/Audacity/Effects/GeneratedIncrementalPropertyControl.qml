/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.UiComponents

IncrementalPropertyControl {
    id: root

    property var parameterData: null

    signal gestureStarted
    signal gestureEnded
    signal valueCommitted(double newValue)

    currentValue: parameterData ? parameterData.currentValue : 0
    minValue: parameterData ? parameterData.minValue : 0
    maxValue: parameterData ? parameterData.maxValue : 1
    step: parameterData && parameterData.stepSize > 0 ? parameterData.stepSize : 0.01
    decimals: parameterData ? parameterData.numDecimals : 2
    measureUnitsSymbol: parameterData ? parameterData.units : ""
    enabled: parameterData ? !parameterData.isReadOnly : false

    property bool isEditing: false

    // Guard against re-entry when the model echoes back the same value.
    property double lastCommittedValue: NaN

    onActiveFocusChanged: {
        if (activeFocus && !isEditing) {
            isEditing = true
            gestureStarted()
        } else if (!activeFocus && isEditing) {
            isEditing = false
            gestureEnded()
        }
    }

    onValueEdited: function (newValue) {
        if (newValue === lastCommittedValue) {
            return
        }
        lastCommittedValue = newValue

        if (!isEditing) {
            isEditing = true
            gestureStarted()
        }
        valueCommitted(newValue)
        if (!activeFocus && isEditing) {
            isEditing = false
            gestureEnded()
        }
    }
}
