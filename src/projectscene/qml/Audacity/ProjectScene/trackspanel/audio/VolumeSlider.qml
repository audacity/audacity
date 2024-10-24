import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

StyledSlider {
    id: root

    Layout.fillWidth: true

    property double snapPoint: 0.0
    property double snapRange: 2.0

    from: -60.0
    to: 12.0

    QtObject {
        id: prv

        property bool dragActive: false
        property real handleWidth: root.handle ? root.handle.width : 0
        property real innerMargin: handleWidth / 2
    }

    VolumeTooltip {
        id: tooltip

        parent: root.handle
        volume: root.value
    }

    // We have to reimplement dragging to allow the tooltip
    // to stay on when the mouse is moved outside of the component
    MouseArea {
        id: mouseArea

        anchors.fill: parent

        hoverEnabled: true

        onPressed: {
            prv.dragActive = true
            root.value = sliderValue()
            tooltip.show(true)
        }

        onReleased: {
            prv.dragActive = false
            if (!containsMouse) {
                tooltip.hide(true)
            }
        }

        onEntered: {
            tooltip.show()
        }

        onExited: {
            if (!prv.dragActive) {
                tooltip.hide(true)
            }
        }

        onPositionChanged: {
            if (!prv.dragActive) {
                return
            }

            let value = sliderValue()
            if (Math.abs(value - snapPoint) < snapRange) {
                value = snapPoint
            }

            root.value = value
        }

        function sliderValue() {
            let relativePos = (mouseX - prv.innerMargin) / (width - prv.handleWidth)
            relativePos = Math.max(0, Math.min(1, relativePos))
            return relativePos * (root.to - root.from) + root.from
        }
    }
}
