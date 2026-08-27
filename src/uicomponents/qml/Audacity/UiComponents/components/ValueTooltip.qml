/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.Ui
import Muse.UiComponents

StyledPopupView {
    id: root

    property var value
    property int decimalPlaces: 1
    property string unitText: ""
    property string format: "%1"

    property string text: {
        const v = Number(root.value)
        return (isFinite(v) ? v.toFixed(root.decimalPlaces) : root.value) + root.unitText
    }
    property string sizingText: root.text

    property int showDelay: ui.theme.tooltipDelay
    property int hideDelay: ui.theme.tooltipDelay

    padding: 8
    margins: 8
    contentWidth: Math.ceil(Math.max(fontMetrics.advanceWidth(root.formatted(root.sizingText)), fontMetrics.advanceWidth(valueLabel.text)))
    contentHeight: Math.ceil(fontMetrics.height)

    placementPolicies: PopupView.PreferAbove
    openPolicies: PopupView.NoActivateFocus

    StyledTextLabel {
        id: valueLabel

        anchors.fill: parent
        text: root.formatted(root.text)
    }

    FontMetrics {
        id: fontMetrics

        font: valueLabel.font
    }

    Connections {
        target: root.parent

        function onXChanged() {
            root.repositionWindowIfNeed()
        }

        function onYChanged() {
            root.repositionWindowIfNeed()
        }
    }

    Timer {
        id: openTimer

        interval: root.showDelay
        repeat: false

        onTriggered: {
            root.open()
        }
    }

    Timer {
        id: closeTimer

        interval: root.hideDelay
        repeat: false

        onTriggered: {
            root.close()
        }
    }

    function formatted(value) {
        if (!root.format.includes("%1")) {
            console.warn("ValueTooltip: format is missing the %1 placeholder:", root.format)
            return root.format + value
        }
        return root.format.arg(value)
    }

    function show(noDelay = false) {
        if (noDelay) {
            root.open()
        } else {
            openTimer.restart()
        }
        closeTimer.stop()
    }

    function hide(noDelay = false) {
        if (noDelay) {
            root.close()
        } else {
            closeTimer.restart()
        }
        openTimer.stop()
    }
}
