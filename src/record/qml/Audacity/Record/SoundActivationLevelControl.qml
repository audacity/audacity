/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.Playback 1.0

Rectangle {
    id: root

    property bool applyImmediately: false
    property alias listening: levelModel.listening

    property NavigationPanel navigationPanel: null
    property int navigationOrderStart: 0

    function apply() {
        levelModel.apply()
    }

    function setLevel(level) {
        const oldLevel = levelModel.level
        levelModel.level = level
        if (root.applyImmediately && levelModel.level !== oldLevel) {
            levelModel.apply()
        }
    }

    height: content.height + 2 * prv.margins

    color: ui.theme.backgroundSecondaryColor
    border.width: 1
    border.color: ui.theme.strokeColor
    radius: 4

    opacity: enabled ? 1.0 : ui.theme.itemOpacityDisabled

    QtObject {
        id: prv

        readonly property int margins: 12
        readonly property int spacing: 8
        readonly property int levelControlWidth: 120
    }

    SoundActivationLevelModel {
        id: levelModel
    }

    Component.onCompleted: {
        levelModel.init()
    }

    Column {
        id: content

        anchors.left: parent.left
        anchors.right: parent.right
        anchors.top: parent.top
        anchors.margins: prv.margins

        spacing: prv.spacing

        StyledTextLabel {
            id: titleLabel

            text: qsTrc("record", "Activation level")
            horizontalAlignment: Text.AlignLeft
        }

        Row {
            width: parent.width
            spacing: 24

            Item {
                width: parent.width - levelControl.width - parent.spacing
                height: meterColumn.height
                anchors.verticalCenter: parent.verticalCenter

                Column {
                    id: meterColumn

                    anchors.left: parent.left
                    anchors.right: parent.right

                    spacing: 2

                    HorizontalVolumePressureMeter {
                        id: leftVolumePressure

                        height: levelModel.recordingChannelsCount > 1 ? 6 : 14

                        meterModel: levelModel
                        showOverload: false

                        currentVolumePressure: levelModel.leftChannelPressure
                        currentRMS: levelModel.leftChannelRMS
                    }

                    HorizontalVolumePressureMeter {
                        id: rightVolumePressure

                        meterModel: levelModel
                        showOverload: false

                        visible: levelModel.recordingChannelsCount > 1

                        currentVolumePressure: levelModel.rightChannelPressure
                        currentRMS: levelModel.rightChannelRMS
                    }

                    HorizontalVolumePressureRuler {
                        anchors.left: parent.left
                        anchors.right: parent.right
                        anchors.rightMargin: leftVolumePressure.overloadWidth

                        meterModel: levelModel
                    }
                }

                VolumeSlider {
                    id: levelSlider

                    meterModel: levelModel
                    volumeLevel: levelModel.level
                    stepSize: 0.1

                    anchors.left: parent.left
                    anchors.leftMargin: -handleWidth / 2
                    anchors.right: parent.right
                    anchors.rightMargin: -handleWidth / 2 + leftVolumePressure.overloadWidth
                    anchors.top: parent.top
                    anchors.topMargin: -1

                    navigation.panel: root.navigationPanel
                    navigation.order: root.navigationOrderStart
                    navigation.accessible.name: titleLabel.text

                    onVolumeLevelMoved: function (level) {
                        root.setLevel(level)
                    }

                    onDecreaseRequested: {
                        root.setLevel(levelModel.level - 1)
                    }

                    onIncreaseRequested: {
                        root.setLevel(levelModel.level + 1)
                    }
                }
            }

            IncrementalPropertyControl {
                id: levelControl

                width: prv.levelControlWidth
                anchors.verticalCenter: parent.verticalCenter

                currentValue: levelModel.level
                minValue: levelModel.dbRange
                maxValue: 0
                step: 1
                decimals: 1
                measureUnitsSymbol: qsTrc("global", "dB")

                navigation.panel: root.navigationPanel
                navigation.order: root.navigationOrderStart + 1
                navigation.accessible.name: titleLabel.text + " " + currentValue + " " + measureUnitsSymbol

                onValueEdited: function (newValue) {
                    root.setLevel(newValue)
                }
            }
        }
    }
}
