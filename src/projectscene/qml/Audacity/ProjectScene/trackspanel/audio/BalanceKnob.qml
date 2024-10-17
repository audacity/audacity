/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

KnobControl {
    id: root

    isBalanceKnob: true
    from: -100
    to: 100
    stepSize: 1

    BalanceTooltip {
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
}
