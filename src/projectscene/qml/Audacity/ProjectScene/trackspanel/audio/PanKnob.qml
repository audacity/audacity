/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

KnobControl {
    id: root

    isPanKnob: true
    from: -100
    to: 100
    stepSize: 1

    signal newPanRequested(real pan, bool completed)

    onNewValueRequested: function(value) {
        newPanRequested(value, false)
    }

    PanTooltip {
        id: tooltip
        value: root.value
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
