import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

StyledSlider {
    id: root

    Layout.fillWidth: true

    property double volumeLevel: 0
    property double snapPoint: 0.0
    property double snapRange: 2.0

    from: -60.0
    to: 12.0

    VolumeTooltip {
        id: tooltip
        parent: root.handle
        volume: root.value
    }

    MouseArea {
        acceptedButtons: Qt.NoButton
        anchors.fill: parent
        hoverEnabled: true
        onEntered: {
            tooltip.show()
        }
        onExited: {
            tooltip.hide()
        }
    }

    onMoved: {
        if (Math.abs(value - snapPoint) < snapRange) {
            value = snapPoint
        }
    }
}
