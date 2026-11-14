/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

Rectangle {
    id: formatsColumn

    width: 320
    height: parent.height

    color: ui.theme.backgroundSecondaryColor

    property var ffmpegPrefModel: null

    property NavigationSection navigationSection: null
    property int navigationOrderStart: 0
    readonly property int navigationOrderEnd: codecsNavPanel.order

    NavigationPanel {
        id: formatsNavPanel

        name: "FFmpegFormats"
        direction: NavigationPanel.Vertical
        section: formatsColumn.navigationSection
        order: formatsColumn.navigationOrderStart
        enabled: formatsColumn.enabled && formatsColumn.visible
        accessible.name: formatsLabel.text
    }

    NavigationPanel {
        id: codecsNavPanel

        name: "FFmpegCodecs"
        direction: NavigationPanel.Vertical
        section: formatsColumn.navigationSection
        order: formatsNavPanel.order + 1
        enabled: formatsColumn.enabled && formatsColumn.visible
        accessible.name: codecsLabel.text
    }

    RowLayout {

        Layout.preferredWidth: formatsColumn.width
        spacing: 10

        ColumnLayout {

            Layout.fillWidth: true

            Layout.topMargin: 10
            Layout.leftMargin: 10
            Layout.bottomMargin: 10

            StyledTextLabel {
                id: formatsLabel
                text: qsTrc("export", "Formats")
            }

            FlatButton {
                Layout.fillWidth: true

                text: qsTrc("export", "Show all")

                navigation.panel: formatsNavPanel
                navigation.row: 0

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
                    anchors.margins: 1

                    currentIndex: ffmpegPrefModel.ffmpegFormatIndex
                    scrollBarPolicy: ScrollBar.AlwaysOn

                    model: ffmpegPrefModel.ffmpegFormatList

                    delegate: ListItemBlank {
                        id: formatItem

                        mouseArea.hoverEnabled: false
                        hoverHitColor: "transparent"
                        isSelected: ListView.isCurrentItem

                        navigation.panel: formatsNavPanel
                        //! NOTE Row 0 is taken by the "Show all" button above
                        navigation.row: index + 1
                        navigation.accessible.name: modelData
                        navigation.accessible.row: index
                        navigation.onActiveChanged: {
                            if (formatItem.navigation.active) {
                                formatItem.scrollIntoView()
                            }
                        }

                        onClicked: {
                            ffmpegPrefModel.setFFmpegFormat(modelData)
                        }

                        StyledTextLabel {
                            anchors.fill: parent
                            anchors.margins: 8
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
                id: codecsLabel
                text: qsTrc("export", "Codecs")
            }

            FlatButton {
                Layout.fillWidth: true

                text: qsTrc("export", "Show all")

                navigation.panel: codecsNavPanel
                navigation.row: 0

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
                    anchors.margins: 1

                    currentIndex: ffmpegPrefModel.ffmpegCodecIndex
                    scrollBarPolicy: ScrollBar.AlwaysOn

                    model: ffmpegPrefModel.ffmpegCodecList

                    delegate: ListItemBlank {
                        id: codecItem

                        mouseArea.hoverEnabled: false
                        hoverHitColor: "transparent"
                        isSelected: ListView.isCurrentItem

                        navigation.panel: codecsNavPanel
                        //! NOTE Row 0 is taken by the "Show all" button above
                        navigation.row: index + 1
                        navigation.accessible.name: modelData
                        navigation.accessible.row: index
                        navigation.onActiveChanged: {
                            if (codecItem.navigation.active) {
                                codecItem.scrollIntoView()
                            }
                        }

                        onClicked: {
                            ffmpegPrefModel.setFFmpegCodec(modelData)
                        }

                        StyledTextLabel {
                            anchors.fill: parent
                            anchors.margins: 8
                            horizontalAlignment: Text.AlignLeft

                            text: modelData
                        }
                    }
                }
            }
        }
    }
}
