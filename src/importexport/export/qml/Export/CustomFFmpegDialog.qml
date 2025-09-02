/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.UiComponents

import Audacity.Export 1.0
import "internal"

StyledDialogView {
    id: root

    title: qsTrc("export", "Custom FFmpeg options")

    contentWidth: 880
    // PRESETS BAR TO BE IMPLEMENTED (44 in height)
    contentHeight: 665 - 44

    CustomFFmpegPreferencesModel {
        id: ffmpegPrefModel
    }

    Component.onCompleted: {
        ffmpegPrefModel.init()
    }

    ColumnLayout {
        id: mainColumn

        spacing: 0

        // PRESETS BAR PLACEHOLDER
        // Rectangle {
        //     width: root.contentWidth
        //     height: 44

        //     color: ui.theme.backgroundPrimaryColor
        // }

        // ENABLE WHEN PRESETS BAR IS VISIBLE
        // SeparatorLine {}

        RowLayout {

            width: root.contentWidth
            height: 575

            spacing: 0

            FormatAndCodecSection {
                ffmpegPrefModel: ffmpegPrefModel
            }

            SeparatorLine {
                orientation: Qt.Vertical
            }

            Rectangle {

                width: 560
                height: parent.height

                color: ui.theme.backgroundPrimaryColor

                ColumnLayout {

                    anchors {
                        fill: parent
                        leftMargin: 20
                        rightMargin: 20
                        topMargin: 30
                        bottomMargin: 30
                    }

                    GeneralOptionsSection {
                        ffmpegPrefModel: ffmpegPrefModel
                    }

                    Item {
                        // spacer
                        Layout.fillHeight: true
                    }

                    SeparatorLine {}

                    Item {
                        // spacer
                        Layout.fillHeight: true
                    }

                    FLACOptionsSection {
                        ffmpegPrefModel: ffmpegPrefModel
                    }

                    Item {
                        // spacer
                        Layout.fillHeight: true
                    }

                    SeparatorLine {}

                    Item {
                        // spacer
                        Layout.fillHeight: true
                    }

                    MPEGOptionsSection {
                        ffmpegPrefModel: ffmpegPrefModel
                    }
                }
            }
        }

        SeparatorLine {}

        ButtonBox {
            id: buttonBox

            Layout.fillWidth: true

            padding: 8

            FlatButton {
                id: cancelBtn

                minWidth: 80

                text: qsTrc("global", "Cancel")
                buttonRole: ButtonBoxModel.RejectRole
                buttonId: ButtonBoxModel.Cancel

                onClicked: root.reject()
            }

            FlatButton {
                id: okBtn

                minWidth: 80

                text: qsTrc("global", "OK")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.Apply
                accentButton: true

                onClicked: root.accept()
            }
        }
    }
}
