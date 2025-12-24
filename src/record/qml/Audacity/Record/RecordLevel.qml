/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.UiComponents 1.0
import Muse.Ui 1.0

import Audacity.Record 1.0
import Audacity.Playback 1.0

import "internal"

FlatButton {
    id: root

    property alias volumeLevel: popup.volumeLevel

    property alias leftCurrentVolumePressure: popup.leftCurrentVolumePressure
    property alias leftCurrentRMS: popup.leftCurrentRMS

    property alias rightCurrentVolumePressure: popup.rightCurrentVolumePressure
    property alias rightCurrentRMS: popup.rightCurrentRMS

    property int meterStyle: PlaybackMeterStyle.Default

    property NavigationPanel navigationPanel: null
    property int navigationOrder: 0

    property int recordingChannelsCount: 0
    property bool isInputMonitoringOn: false
    property bool isMicMeteringOn: false

    signal volumeLevelChangeRequested(var level)
    signal isInputMonitoringOnChangeRequested(bool enable)
    signal isMicMeteringOnChangeRequested(bool enable)
    signal isPopupOpened(bool opened)

    accentButton: popup.isOpened

    iconFont: ui.theme.toolbarIconsFont

    navigation.panel: navigationPanel
    navigation.name: text
    navigation.order: navigationOrder
    isClickOnKeyNavTriggered: false
    navigation.onTriggered: {
        togglePopupOpened()
    }

    mouseArea.acceptedButtons: Qt.LeftButton | Qt.RightButton

    function togglePopupOpened() {
        if (popup.isOpened) {
            popup.close()
        } else {
            popup.open()
        }
    }

    onClicked: function(mouse) {
        togglePopupOpened()
    }

    Connections {
        target: root.mouseArea

        enabled: !popup.isOpened

        function onPressAndHold() {
            if (popup.isOpened) {
                return
            }

            root.togglePopupOpened()
        }
    }

    RecordLevelPopup {
        id: popup

        recordingChannelsCount: root.recordingChannelsCount
        isInputMonitoringOn: root.isInputMonitoringOn
        isMicMeteringOn: root.isMicMeteringOn

        onVolumeLevelChangeRequested: function(level) {
            root.volumeLevelChangeRequested(level)
        }

        onIsInputMonitoringOnChangeRequested: function(enable) {
            root.isInputMonitoringOnChangeRequested(enable)
        }

        onIsMicMeteringOnChangeRequested: function(enable) {
            root.isMicMeteringOnChangeRequested(enable)
        }

        onOpened: {
            root.isPopupOpened(true)
        }

        onClosed: {
            root.isPopupOpened(false)
        }
    }
}
