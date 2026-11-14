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
    // Strip the group separators formatReal adds: the base control reformats
    // the field on every keystroke, and "1,234" breaks the validator
    currentText: ui.df.formatReal(root.currentValue ? root.currentValue : 0.0, root.decimals).split(Qt.locale().groupSeparator).join("")
    // IntInputValidator (used when decimals is 0) has int bounds that wrap
    // when fed e.g. +/-FLT_MAX; the double validator takes any range
    function clampBoundToValidator(v) {
        return root.decimals > 0 ? v : Math.max(-2147483648, Math.min(2147483647, v))
    }

    minValue: parameterData ? clampBoundToValidator(parameterData.minValue) : 0
    maxValue: parameterData ? clampBoundToValidator(parameterData.maxValue) : 1
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
