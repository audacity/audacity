/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
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
                id: formatAndCodecSection

                ffmpegPrefModel: ffmpegPrefModel

                navigationSection: root.navigationSection
                navigationOrderStart: 1
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
                        id: generalOptionsSection

                        ffmpegPrefModel: ffmpegPrefModel

                        navigationSection: root.navigationSection
                        navigationOrderStart: formatAndCodecSection.navigationOrderEnd + 1
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
                        id: flacOptionsSection

                        ffmpegPrefModel: ffmpegPrefModel

                        navigationSection: root.navigationSection
                        navigationOrderStart: generalOptionsSection.navigationOrderEnd + 1
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
                        id: mpegOptionsSection

                        ffmpegPrefModel: ffmpegPrefModel

                        navigationSection: root.navigationSection
                        navigationOrderStart: flacOptionsSection.navigationOrderEnd + 1
                    }
                }
            }
        }

        SeparatorLine {}

        ButtonBox {
            id: buttonBox

            Layout.fillWidth: true

            padding: 8

            navigationPanel.section: root.navigationSection
            navigationPanel.order: mpegOptionsSection.navigationOrderEnd + 1

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
