/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick

import Muse.UiComponents

import Audacity.UiComponents
import Audacity.Playback

BaseSection {
    id: root

    // No translation needed
    title: "ASIO"
    spacing: 16

    property var apiModel: null

    PlaybackStateModel {
        id: playbackState
    }

    Component.onCompleted: {
        playbackState.init()
    }

    Row {
        width: parent.width
        spacing: root.spacing

        CheckBox {
            width: root.columnWidth
            anchors.verticalCenter: parent.verticalCenter

            text: qsTrc("preferences", "Use device sample rate")

            enabled: !playbackState.isPlaying

            checked: apiModel.asioUseDeviceSampleRate

            navigation.name: "AsioUseDeviceSampleRateBox"
            navigation.panel: root.navigation
            navigation.row: 1
            navigation.column: 0

            onClicked: {
                apiModel.setAsioUseDeviceSampleRate(!checked)
            }
        }

        FlatButton {
            text: qsTrc("preferences", "Driver settings")

            enabled: !playbackState.isPlaying

            navigation.name: "AsioDriverSettingsButton"
            navigation.panel: root.navigation
            navigation.row: 1
            navigation.column: 1

            onClicked: {
                apiModel.showAsioControlPanel()
            }
        }
    }
}
