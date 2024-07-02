/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.Playback 1.0
import Audacity.Record 1.0

import "internal"
import "components"

Item {
    id: root

    property bool floating: false

    property int maximumWidth: 0
    property int maximumHeight: 0

    property alias navigationPanel: view.navigationPanel

    implicitWidth: maximumWidth
    implicitHeight: view.height

    PStyledToolBarView {
        id: view

        anchors.verticalCenter: parent.verticalCenter
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.rightMargin: 4 + customizeButton.width + 8

        widthDependsOnContent: false

        rowHeight: 48

        model: PlaybackToolBarModel {}

        sourceComponentCallback: function(type) {
            switch(type) {
            case PlaybackToolBarModel.PLAYBACK_CONTROL: return controlComp
            case PlaybackToolBarModel.PLAYBACK_LEVEL: return playbackLevelComp
            case PlaybackToolBarModel.PLAYBACK_TIME: return playbackTimeComp
            case PlaybackToolBarModel.PLAYBACK_BPM: return playbackBPMComp
            case PlaybackToolBarModel.PLAYBACK_TIME_SIGNATURE: return playbackTimeSignatureComp
            case PlaybackToolBarModel.RECORD_LEVEL: return recordLevelComp
            case PlaybackToolBarModel.PROJECT_CONTROL: return projectControlComp
            }

            return null
        }

        Component {
            id: controlComp

            StyledToolBarItem {
                width: 32
                height: width

                iconColor: Boolean(itemData) ? itemData.iconColor : ui.theme.fontPrimaryColor
                accentColor: Boolean(itemData) ? itemData.backgroundColor : ui.theme.buttonColor
                accentButton: Boolean(itemData) ? itemData.selected : false
            }
        }

        Component {
            id: projectControlComp

            StyledToolBarItem {
                width: 28
                height: width
            }
        }

        Component {
            id: playbackLevelComp

            PlaybackLevel {
                property var itemData: null

                width: 240
                height: 28

                volumeLevel: Boolean(itemData) ? itemData.level : 0
                leftCurrentVolumePressure: Boolean(itemData) ? itemData.leftChannelPressure : 0
                rightCurrentVolumePressure: Boolean(itemData) ? itemData.rightChannelPressure : 0

                navigationPanel: root.navigationPanel
                navigationOrder: 2

                onVolumeLevelChangeRequested: function(level) {
                    itemData.level = level
                }
            }
        }

        Component {
            id: playbackTimeComp

            Timecode {
                property var itemData: null

                value: Boolean(itemData) ? itemData.currentValue : 0

                sampleRate: Boolean(itemData) ? itemData.sampleRate : 0
                tempo: Boolean(itemData) ? itemData.tempo : 0
                upperTimeSignature: Boolean(itemData) ? itemData.upperTimeSignature : 0
                lowerTimeSignature: Boolean(itemData) ? itemData.lowerTimeSignature : 0

                currentFormat: Boolean(itemData) ? itemData.currentFormat : 0

                onValueChangeRequested: function(newValue) {
                    if (!Boolean(itemData)) {
                        return
                    }

                    itemData.currentValue = newValue
                }

                onCurrentFormatChanged: {
                    if (!Boolean(itemData)) {
                        return
                    }

                    itemData.currentFormat = currentFormat
                }
            }
        }

        Component {
            id: playbackBPMComp

            BPM {
                property var itemData: null

                value: Boolean(itemData) ? itemData.currentValue : 0

                onValueChangeRequested: function(newValue) {
                    if (!Boolean(itemData)) {
                        return
                    }

                    itemData.currentValue = newValue
                }
            }
        }

        Component {
            id: playbackTimeSignatureComp

            TimeSignature {
                property var itemData: null

                upper: Boolean(itemData) ? itemData.upper : 0
                lower: Boolean(itemData) ? itemData.lower : 0

                onUpperChangeRequested: function(newValue) {
                    if (!Boolean(itemData)) {
                        return
                    }

                    itemData.upper = newValue
                }

                onLowerChangeRequested: function(newValue) {
                    if (!Boolean(itemData)) {
                        return
                    }

                    itemData.lower = newValue
                }
            }
        }

        Component {
            id: recordLevelComp

            RecordLevel {
                property var itemData: null

                width: 28
                height: width

                icon: Boolean(itemData) ? itemData.icon : IconCode.NONE

                toolTipTitle: Boolean(itemData) ? itemData.title : ""
                toolTipDescription: Boolean(itemData) ? itemData.description : ""

                volumeLevel: Boolean(itemData) ? itemData.level : 0
                leftCurrentVolumePressure: Boolean(itemData) ? itemData.leftChannelPressure : 0
                rightCurrentVolumePressure: Boolean(itemData) ? itemData.rightChannelPressure : 0

                navigationPanel: root.navigationPanel
                navigationOrder: 1

                onVolumeLevelChangeRequested: function(level) {
                    itemData.level = level
                }
            }
        }
    }

    FlatButton {
        id: customizeButton

        anchors.right: parent.right
        anchors.rightMargin: 12
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 12

        width: 28
        height: width

        icon: IconCode.SETTINGS_COG
        iconFont: ui.theme.toolbarIconsFont
        toolTipTitle: qsTrc("playback", "Customize toolbar")
        toolTipDescription: qsTrc("playback", "Show/hide toolbar buttons")

        navigation.panel: root.navigationPanel
        navigation.order: 100
        navigation.accessible.name: qsTrc("playback", "Customize toolbar")

        onClicked: {
            customizePopup.toggleOpened()
        }

        PlaybackToolBarCustomisePopup {
            id: customizePopup

            anchorItem: !root.floating ? ui.rootItem : null
        }
    }
}
