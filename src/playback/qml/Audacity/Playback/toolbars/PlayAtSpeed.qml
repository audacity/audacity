/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Playback

Item {
    id: root

    // itemData is assigned by StyledToolBarView's Loader.onLoaded.
    property var itemData: null
    property var navigationPanel: null
    property int navigationOrder: 0

    implicitWidth: contentRow.implicitWidth
    implicitHeight: 28
    width: implicitWidth
    height: implicitHeight

    // Prefer the local model for enablement. itemData.enabled is also consulted
    // when present, but must not leave the control stuck disabled if itemData
    // has not been assigned yet (toolbar Loader timing).
    enabled: model.isEnabled && (!Boolean(itemData) || itemData.enabled)

    PlayAtSpeedModel {
        id: model

        Component.onCompleted: {
            model.init()
        }
    }

    RowLayout {
        id: contentRow

        anchors.verticalCenter: parent.verticalCenter
        spacing: 6

        StyledTextLabel {
            Layout.preferredWidth: 42
            horizontalAlignment: Text.AlignRight
            text: (Math.round(model.speed * 100) / 100).toFixed(2) + "x"
            opacity: (root.enabled && model.isEnabled) ? 1.0 : ui.theme.itemOpacityDisabled

            MouseArea {
                anchors.fill: parent
                enabled: root.enabled && model.isEnabled
                hoverEnabled: true
                onDoubleClicked: {
                    model.resetSpeed()
                }
                onEntered: {
                    ui.tooltip.show(parent, qsTrc("playback", "Playback speed"),
                                    qsTrc("playback", "Double-click to reset to 1.00x"))
                }
                onExited: {
                    ui.tooltip.hide(parent)
                }
            }
        }

        Slider {
            id: speedSlider

            Layout.preferredWidth: 100
            Layout.preferredHeight: 28

            from: 0.01
            to: 3.0
            stepSize: 0.01
            value: model.speed
            enabled: root.enabled && model.isEnabled
            wheelEnabled: true

            onMoved: {
                model.speed = value
            }

            background: Rectangle {
                x: speedSlider.leftPadding
                y: speedSlider.topPadding + speedSlider.availableHeight / 2 - height / 2
                implicitWidth: 100
                implicitHeight: 4
                width: speedSlider.availableWidth
                height: implicitHeight
                radius: 2
                color: ui.theme.buttonColor

                Rectangle {
                    width: speedSlider.visualPosition * parent.width
                    height: parent.height
                    color: ui.theme.accentColor
                    radius: 2
                }
            }

            handle: Rectangle {
                x: speedSlider.leftPadding + speedSlider.visualPosition * (speedSlider.availableWidth - width)
                y: speedSlider.topPadding + speedSlider.availableHeight / 2 - height / 2
                implicitWidth: 14
                implicitHeight: 14
                radius: 7
                color: ui.theme.fontPrimaryColor
                border.color: ui.theme.strokeColor
            }
        }

        FlatButton {
            Layout.preferredWidth: 24
            Layout.preferredHeight: 28

            text: "−"
            backgroundRadius: 2
            enabled: root.enabled && model.isEnabled && model.speed > 0.01

            toolTipTitle: qsTrc("playback", "Decrease playback speed")

            navigation.name: "DecreasePlaySpeed"
            navigation.panel: root.navigationPanel
            navigation.order: root.navigationOrder

            onClicked: {
                model.decreaseSpeed()
            }
        }

        FlatButton {
            Layout.preferredWidth: 24
            Layout.preferredHeight: 28

            text: "+"
            backgroundRadius: 2
            enabled: root.enabled && model.isEnabled && model.speed < 3.0

            toolTipTitle: qsTrc("playback", "Increase playback speed")

            navigation.name: "IncreasePlaySpeed"
            navigation.panel: root.navigationPanel
            navigation.order: root.navigationOrder + 1

            onClicked: {
                model.increaseSpeed()
            }
        }
    }
}
