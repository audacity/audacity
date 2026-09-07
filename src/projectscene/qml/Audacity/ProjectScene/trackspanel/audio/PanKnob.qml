/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.UiComponents

KnobControl {
    id: root

    isPanKnob: true
    from: -100
    to: 100
    stepSize: 1

    signal newPanRequested(real pan, bool completed)

    onNewValueRequested: function (value) {
        newPanRequested(value, false)
    }

    ValueTooltip {
        id: tooltip

        format: qsTrc("projectscene", "Pan: %1")
        unitText: root.value < 0 ? "L" : root.value > 0 ? "R" : ""
        sizingText: "100R"

        decimalPlaces: 0

        value: Math.abs(root.value)
    }

    onMousePressed: {
        tooltip.show(true)
    }

    onMouseEntered: {
        tooltip.show()
    }

    onMouseExited: {
        tooltip.hide(true)
    }

    onMouseReleased: {
        newPanRequested(root.value, true)
    }
}
