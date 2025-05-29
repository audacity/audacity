
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

    property real handleWidth: 16
    readonly property real handleHeight: handleWidth

    signal volumeLevelMoved(var level)
    signal handlePressed()

    from: -60
    to: 0
    value: root.volumeLevel
    stepSize: 0.1
    orientation: Qt.Vertical
    wheelEnabled: true

    height: parent.height
    width: handleWidth

    opacity: enabled ? 1.0 : ui.theme.itemOpacityDisabled

    signal increaseRequested()
    signal decreaseRequested()

    QtObject {
        id: prv

        readonly property real rulerLineHeight: root.height - root.handleHeight

        readonly property int rulerXPos: root.width / 2

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
            case NavigationEvent.Down:
                root.decreaseRequested()
                event.accepted = true
                break
            case NavigationEvent.Up:
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
        x: prv.rulerXPos - root.handleWidth / 2
        y: (1.0 - root.position) * prv.rulerLineHeight
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
                prv.dragStartOffset = mouse.y
                root.handlePressed()
            }

            onPositionChanged: function(mouse)  {
                let mousePosInRoot = mapToItem(root, 0, mouse.y - prv.dragStartOffset).y
                if (prv.rulerLineHeight === 0) {
                    return;
                }
                let proportionFromTop = mousePosInRoot / prv.rulerLineHeight
                let clampedProportion = Math.max(0.0, Math.min(proportionFromTop, 1.0))
                let newRootPosition = 1.0 - clampedProportion

                let localNewValue = root.valueAt(newRootPosition)
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
                opacity: 0.7
            }

        }
    }

    onMoved: {
        navigation.requestActiveByInteraction()
        root.volumeLevelMoved(value)
    }
}
