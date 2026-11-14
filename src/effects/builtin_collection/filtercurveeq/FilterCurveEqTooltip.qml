import QtQuick

import Muse.Ui
import Muse.UiComponents

StyledPopupView {
    id: root

    padding: 8
    margins: 8
    contentWidth: contentRect.width
    contentHeight: contentRect.height

    placementPolicies: PopupView.PreferAbove
    openPolicies: PopupView.NoActivateFocus

    property real freq: 0
    property real gain: 0

    property rect contentRect: {
        let rect = fontMetrics.boundingRect("00.0 kHz")
        rect.height = rect.height * 2
        return rect
    }

    property int decimalPlaces: 1

    property int showDelay: ui.theme.tooltipDelay
    property int hideDelay: ui.theme.tooltipDelay

    function freqLabel(f) {
        if (f >= 1000) {
            return (f / 1000).toFixed(root.decimalPlaces) + " kHz"
        }
        return Math.round(f) + " Hz"
    }

    Item {
        id: content

        anchors.fill: parent

        StyledTextLabel {
            id: label

            anchors.horizontalCenter: parent.horizontalCenter
            text: root.freqLabel(root.freq) + "\n" + root.gain.toFixed(root.decimalPlaces) + " dB"
        }
    }

    FontMetrics {
        id: fontMetrics

        font: label.font
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
