/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.UiComponents 1.0
import Audacity.Export 1.0

BaseSection {
    id: root

    navigation.direction: NavigationPanel.Horizontal

    CustomFFmpegPreferencesModel {
        id: ffmpegPrefModel
    }

    Column {
        spacing: 12

        Row {
            StyledTextLabel {
                text: qsTrc("preferences", "FFmpeg library version: ")
            }

            StyledTextLabel {
                text: ffmpegPrefModel.ffmpegVersion
            }
        }

        Row {
            spacing: 8

            TextInputField {
                id: formatField

                width: 223

                currentText: ffmpegPrefModel.ffmpegLibraryPath ?? qsTrc("preferences", "FFmpeg library not found")

                property var newLibPath

                onTextEditingFinished: function(newTextValue) {
                    let result = ffmpegPrefModel.setFFmpegLibraryPath(newTextValue)
                    currentText = ffmpegPrefModel.ffmpegLibraryPath
                    inputField.text = ffmpegPrefModel.ffmpegLibraryPath
                }
            }

            FlatButton {
                id: downloadBtn

                width: 182

                text: qsTrc("export", "Download FFmpeg")

                navigation.panel: root.navigation
                navigation.order: 1

                onClicked: {
                    api.launcher.openUrl("https://support.audacityteam.org/basics/installing-ffmpeg")
                }
            }

            FlatButton {
                width: 178

                text: qsTrc("export", "Locate existing installation")

                navigation.panel: root.navigation
                navigation.order: downloadBtn.navigation.order + 1

                onClicked: {
                    ffmpegPrefModel.locateFFmpegLibrary()
                }
            }
        }
    }
}
