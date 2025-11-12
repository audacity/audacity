/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Muse.UiComponents

Rectangle {
    id: formatsColumn

    width: 320
    height: parent.height

    color: ui.theme.backgroundSecondaryColor

    property var ffmpegPrefModel: null

    RowLayout {

        Layout.preferredWidth: formatsColumn.width
        spacing: ui.theme.extra.space_10

        ColumnLayout {

            Layout.fillWidth: true

            Layout.topMargin: 10
            Layout.leftMargin: 10
            Layout.bottomMargin: 10

            StyledTextLabel {
                text: qsTrc("export", "Formats")
            }

            FlatButton {
                Layout.fillWidth: true

                text: qsTrc("export", "Show all")

                onClicked: {
                    ffmpegPrefModel.fetchAllFormats()
                }
            }

            Rectangle {
                width: 142
                height: 502

                color: ui.theme.textFieldColor
                border.width: 1
                border.color: ui.theme.strokeColor

                StyledListView {
                    anchors.fill: parent
                    anchors.margins: ui.theme.extra.space_1

                    currentIndex: ffmpegPrefModel.ffmpegFormatIndex
                    scrollBarPolicy: ScrollBar.AlwaysOn

                    model: ffmpegPrefModel.ffmpegFormatList

                    delegate: ListItemBlank {
                        mouseArea.hoverEnabled: false
                        hoverHitColor: "transparent"
                        isSelected: ListView.isCurrentItem

                        onClicked: {
                            ffmpegPrefModel.setFFmpegFormat(modelData)
                        }

                        StyledTextLabel {
                            anchors.fill: parent
                            anchors.margins: ui.theme.extra.space_8
                            horizontalAlignment: Text.AlignLeft

                            text: modelData
                        }
                    }
                }
            }
        }

        ColumnLayout {

            Layout.fillWidth: true
            Layout.topMargin: 10
            Layout.leftMargin: 5
            Layout.bottomMargin: 10
            Layout.rightMargin: 10

            StyledTextLabel {
                text: qsTrc("export", "Codecs")
            }

            FlatButton {
                Layout.fillWidth: true

                text: qsTrc("export", "Show all")

                onClicked: {
                    ffmpegPrefModel.fetchAllCodecs()
                }
            }

            Rectangle {
                width: 142
                height: 502

                color: ui.theme.textFieldColor
                border.width: 1
                border.color: ui.theme.strokeColor

                StyledListView {
                    anchors.fill: parent
                    anchors.margins: ui.theme.extra.space_1

                    currentIndex: ffmpegPrefModel.ffmpegCodecIndex
                    scrollBarPolicy: ScrollBar.AlwaysOn

                    model: ffmpegPrefModel.ffmpegCodecList

                    delegate: ListItemBlank {
                        mouseArea.hoverEnabled: false
                        hoverHitColor: "transparent"
                        isSelected: ListView.isCurrentItem

                        onClicked: {
                            ffmpegPrefModel.setFFmpegCodec(modelData)
                        }

                        StyledTextLabel {
                            anchors.fill: parent
                            anchors.margins: ui.theme.extra.space_8
                            horizontalAlignment: Text.AlignLeft

                            text: modelData
                        }
                    }
                }
            }
        }
    }
}
