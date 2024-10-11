import QtQuick

import Muse.Ui
import Muse.UiComponents

StyledPopupView {
    id: root

    padding: 8
    margins: 8
    contentWidth: contentRect.width
    contentHeight: contentRect.height

    placement: PopupView.PreferAbove
    openPolicies: PopupView.NoActivateFocus

    property double volume
    property rect contentRect: fontMetrics.boundingRect("-60.0dB")

    Item {
        id: content

        anchors.fill: parent

        StyledTextLabel {
            anchors.right: parent.right
            text: {
                let value = root.volume.toFixed(1);
                return `${value}dB`
            }
        }
    }

    FontMetrics {
        id: fontMetrics

        font: ui.theme.defaultFont
    }

    Timer {
        id: openTimer

        interval: ui.theme.tooltipDelay
        repeat: false

        onTriggered: {
            open()
        }
    }

    Timer {
        id: closeTimer

        interval: ui.theme.tooltipDelay
        repeat: false

        onTriggered: {
            close()
        }
    }

    function show() {
        openTimer.restart()
        closeTimer.stop()
    }

    function hide() {
        closeTimer.restart()
        openTimer.stop()
    }
}
