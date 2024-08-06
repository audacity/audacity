/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

StyledPopupView {
    id: root
    placement: PopupView.Above
    openPolicies: PopupView.NoActivateFocus
    padding: 8
    margins: 8
    contentWidth: contentRect.width
    contentHeight: contentRect.height

    property rect contentRect: fontMetrics.boundingRect("Pan: 100R")
    property double value: 0

    Item {
        anchors.fill: parent
        id: content
        Text {
            anchors.fill: parent
            text: "Pan:"
        }

        Text {
            anchors.right: parent.right
            text: {
                let value = Math.round(root.value);
                let direction = value < 0 ? 'L' : value > 0 ? 'R' : '';
                return `${Math.abs(value)}${direction}`;
            }
        }
    }

    FontMetrics {
        id: fontMetrics
        font: content.font
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
