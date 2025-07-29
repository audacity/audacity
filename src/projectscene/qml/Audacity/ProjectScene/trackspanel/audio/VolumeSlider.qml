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
    property bool shiftPressed: false

    signal newVolumeRequested(real volume, bool completed)

    from: -60.0
    to: 12.0

    onValueChanged: {
        newVolumeRequested(value, false)
    }

    QtObject {
        id: prv

        property bool dragActive: false
        property real handleWidth: root.handle ? root.handle.width : 0
        property real innerMargin: handleWidth / 2
        property real startPos: 0.0
        property real startFineValue: 0.0
    }

    Connections {
        target: Qt.application

        function onStateChanged() {
            if (Qt.application.state !== Qt.ApplicationActive) {
                prv.dragActive = false
                tooltip.hide(true)
            }
        }
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
            prv.startPos = mouseX
            prv.startFineValue = root.value
        }

        onDoubleClicked: {
            root.newVolumeRequested(root.value = 0, true)
        }

        onReleased: {
            prv.dragActive = false
            if (!containsMouse) {
                tooltip.hide(true)
            }
            root.newVolumeRequested(root.value, true)
        }

        onEntered: {
            tooltip.show()
        }

        onExited: {
            if (!prv.dragActive) {
                tooltip.hide(true)
            }
        }

        onPositionChanged: function(e) {
            if (!prv.dragActive) {
                return
            }

            if ((e.modifiers & (Qt.ShiftModifier))) {
                if (!root.shiftPressed) {
                    root.shiftPressed = true
                    prv.startPos = mouseX
                    prv.startFineValue = root.value
                }

                root.value = fineSliderValue()
            } else {
                if (root.shiftPressed) {
                    root.shiftPressed = false
                    prv.startPos = mouseX
                    prv.startFineValue = root.value
                }
                root.value = sliderValue()
            }
        }

        function sliderValue() {
            let relativePos = (mouseX - prv.innerMargin) / (width - prv.handleWidth)
            relativePos = Math.max(0, Math.min(1, relativePos))
            return relativePos * (root.to - root.from) + root.from
        }

        function fineSliderValue() {
            let step = 2 * (mouseX - prv.startPos) / (root.to - root.from)
            return prv.startFineValue + step
        }
    }
}
