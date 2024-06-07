/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.Playback 1.0
import Audacity.Record 1.0

import "internal"

Item {
    id: root

    property bool floating: false

    property int maximumWidth: 0
    property int maximumHeight: 0

    property alias navigationPanel: view.navigationPanel

    width: view.width + /*spacing*/ 4 + customizeButton.width
    height: 48 // todo

    StyledToolBarView {
        id: view

        anchors.verticalCenter: parent.verticalCenter

        rowHeight: 48

        model: PlaybackToolBarModel {
            id: toolbarModel
        }

        sourceComponentCallback: function(type) {
            switch(type) {
            case PlaybackToolBarModel.PLAYBACK_CONTROL: return controlComp
            case PlaybackToolBarModel.PLAYBACK_LEVEL: return playbackLevelComp
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

        anchors.margins: 4
        anchors.left: view.right
        anchors.verticalCenter: root.verticalCenter

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
