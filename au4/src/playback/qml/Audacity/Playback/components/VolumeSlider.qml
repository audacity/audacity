/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

Slider {
    id: root

    property real volumeLevel: 0.0
    property real readableVolumeLevel: Math.round(root.volumeLevel * 10) / 10

    property alias navigation: navCtrl

    readonly property real handleWidth: 16
    readonly property real handleHeight: handleWidth

    signal volumeLevelMoved(var level)

    from: -60
    to: 0
    value: root.volumeLevel
    stepSize: 0.1
    orientation: Qt.Horizontal
    wheelEnabled: true

    width: parent.width
    height: handleWidth

    opacity: enabled ? 1.0 : ui.theme.itemOpacityDisabled

    signal increaseRequested()
    signal decreaseRequested()

    QtObject {
        id: prv

        readonly property real rulerLineWidth: root.width - root.handleWidth

        readonly property int rulerYPos: root.height / 2

        property real dragStartOffset: 0.0
    }

    NavigationControl {
        id: navCtrl
        name: root.objectName !== "" ? root.objectName : "VolumeSlider"
        enabled: root.enabled && root.visible

        accessible.role: MUAccessible.Range
        accessible.visualItem: root

        accessible.value: root.readableVolumeLevel
        accessible.minimumValue: root.from
        accessible.maximumValue: root.to
        accessible.stepSize: root.stepSize

        onNavigationEvent: function(event) {
            switch(event.type) {
            case NavigationEvent.Left:
                root.decreaseRequested()
                event.accepted = true
                break
            case NavigationEvent.Right:
                root.increaseRequested()
                event.accepted = true
                break
            }
        }
    }

    background: Item {
        id: bgCanvas

        height: root.height
        width: root.width

        NavigationFocusBorder {
            navigationCtrl: navCtrl
        }
    }

    handle: Item {
        x: root.position * prv.rulerLineWidth
        y: prv.rulerYPos - root.handleHeight / 2
        implicitWidth: root.handleWidth
        implicitHeight: root.handleHeight

        MouseArea {
            anchors.fill: parent

            onDoubleClicked: {
                // Double click resets the volume
                root.volumeLevelMoved(0.0)
            }

            // The MouseArea steals mouse press events from the slider.
            // There is really no way to prevent that.
            // (if you set mouse.accepted to false in the onPressed handler,
            // the MouseArea won't receive doubleClick events).
            // So we use this workaround.

            preventStealing: true // Don't let a Flickable steal the mouse

            onPressed: function(mouse) {
                prv.dragStartOffset = mouse.x
            }

            onPositionChanged: function(mouse)  {
                let mousePosInRoot = mapToItem(root, mouse.x - prv.dragStartOffset, 0).x
                let newPosZeroToOne = mousePosInRoot / prv.rulerLineWidth

                let newPosClamped = Math.max(0.0, Math.min(newPosZeroToOne, 1.0))
                let localNewValue = root.valueAt(newPosClamped)
                root.volumeLevelMoved(localNewValue)
            }
        }

        Rectangle {
            id: handleRect
            anchors.fill: parent
            radius: width / 2
            color: "transparent"
            border.color: ui.theme.fontPrimaryColor

            Rectangle {
                anchors.fill: parent
                anchors.margins: 1
                radius: width / 2
                color: ui.theme.backgroundPrimaryColor
                opacity: 0.3
            }

        }
    }

    onMoved: {
        navigation.requestActiveByInteraction()
        root.volumeLevelMoved(value)
    }
}
