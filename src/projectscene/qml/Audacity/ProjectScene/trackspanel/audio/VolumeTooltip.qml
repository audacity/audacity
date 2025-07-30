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
    closePolicies: PopupView.NoAutoClose

    property double volume
    property rect contentRect: fontMetrics.boundingRect(minValue.toFixed(root.decimalPlaces) + unitText)

    property int decimalPlaces: 1
    property string unitText: "dB"
    property real minValue: -60.0

    Item {
        id: content

        anchors.fill: parent

        StyledTextLabel {
            id: label

            anchors.horizontalCenter: parent.horizontalCenter
            text: {
                let value = root.volume.toFixed(root.decimalPlaces);
                return `${value}${root.unitText}`;
            }
        }
    }

    FontMetrics {
        id: fontMetrics

        font: label.font
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

    function show(noDelay = false) {
        if (noDelay) {
            open()
        } else {
            openTimer.restart()
        }
        closeTimer.stop()
    }

    function hide(noDelay = false) {
        if (noDelay) {
            close()
        } else {
            closeTimer.restart()
        }
        openTimer.stop()
    }
}
