/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.Playback 1.0
import Audacity.Record 1.0
import Audacity.ProjectScene 1.0

import "internal"

Item {
    id: root

    property bool floating: false

    property int maximumWidth: 0
    property int maximumHeight: 0

    property alias navigationPanel: view.navigationPanel

    width: {
        let contentWidth = view.width + prv.customizeButtonSpaceWidth
        return maximumWidth > 0 ? Math.max(contentWidth, maximumWidth) : contentWidth
    }
    height: view.height

    QtObject {
        id: prv

        property int customizeButtonSpaceWidth: 8 /* spacing */ + customizeButton.width + customizeButton.anchors.rightMargin
    }

    StyledToolBarView {
        id: view

        anchors.verticalCenter: parent.verticalCenter

        rowHeight: 48
        separatorHeight: 28
        maximumWidth: root.maximumWidth - prv.customizeButtonSpaceWidth

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
            case PlaybackToolBarModel.SNAP: return snapComp
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

                enabled: Boolean(itemData) ? itemData.enabled : false

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

                mode: TimecodeModeSelector.TimePoint

                sampleRate: Boolean(itemData) ? itemData.sampleRate : 0
                tempo: Boolean(itemData) ? itemData.tempo : 0
                upperTimeSignature: Boolean(itemData) ? itemData.upperTimeSignature : -1
                lowerTimeSignature: Boolean(itemData) ? itemData.lowerTimeSignature : -1

                currentFormat: Boolean(itemData) ? itemData.currentFormat : 0

                enabled: Boolean(itemData) ? itemData.enabled : false

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

                enabled: Boolean(itemData) ? itemData.enabled : false

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

                enabled: Boolean(itemData) ? itemData.enabled : false

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

                enabled: Boolean(itemData) ? itemData.enabled : false

                onVolumeLevelChangeRequested: function(level) {
                    itemData.level = level
                }
            }
        }

        Component {
            id: snapComp

            Snap {
                property var itemData: null

                width: 143
                height: 28

                isSnapEnabled: Boolean(itemData) ? itemData.isSnapEnabled : false
                currentSnapMode: Boolean(itemData) ? itemData.currentValue : ""
                contextMenuModel: Boolean(itemData) ? itemData.availableSnapTypes : null

                enabled: Boolean(itemData) ? itemData.enabled : false

                onSnapEnableChangeRequested: function(enabled) {
                    itemData.isSnapEnabled = enabled
                }

                onHandleMenuItem: function(itemId) {
                    itemData.handleMenuItem(itemId)
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
        toolTipTitle: qsTrc("projectscene", "Customize toolbar")
        toolTipDescription: qsTrc("projectscene", "Show/hide toolbar buttons")

        enabled: view.model.isEnabled

        navigation.panel: root.navigationPanel
        navigation.order: 100
        navigation.accessible.name: qsTrc("projectscene", "Customize toolbar")

        onClicked: {
            customizePopup.toggleOpened()
        }

        PlaybackToolBarCustomisePopup {
            id: customizePopup

            anchorItem: !root.floating ? ui.rootItem : null
        }
    }
}
