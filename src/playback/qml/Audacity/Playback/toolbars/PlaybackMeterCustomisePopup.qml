/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15
import QtQuick.Controls 2.15

import Muse.UiComponents
import Muse.Ui 1.0

import Audacity.Playback 1.0
import Audacity.UiComponents 1.0

StyledPopupView {
    id: root

    objectName: "PlaybackMeterCustomisePopup"

    property var model: null

    contentWidth: 336
    contentHeight: 286

    margins: 12

    ColumnLayout {
        anchors.fill: parent
        spacing: 12

        StyledGroupBox {
            Layout.fillWidth: true
            Layout.preferredHeight: 92

            title: qsTrc("playback", "Position")

            titleSpacing: 4

            value: root.model.meterPosition

            navPanel.name: "PlaybackMeterPosition"
            navPanel.section: root.navigationSection
            navPanel.order: 1
            navPanel.accessible.name: title

            model: [
                {
                    label: qsTrc("playback", "Top bar (horizontal)"),
                    value: PlaybackMeterPosition.TopBar
                },
                {
                    label: qsTrc("playback", "Side bar (vertical)"),
                    value: PlaybackMeterPosition.SideBar
                }
            ]

            onValueChangeRequested: function (value) {
                root.model.meterPosition = value
            }
        }

        RowLayout {
            Layout.fillWidth: true
            Layout.preferredHeight: 120

            spacing: 12

            StyledGroupBox {
                Layout.fillWidth: true
                Layout.preferredWidth: 0 // Force both to start with the same width
                Layout.preferredHeight: 120

                title: qsTrc("Playback", "Meter style")

                titleSpacing: 4

                value: root.model.meterStyle

                navPanel.name: "PlaybackMeterStyle"
                navPanel.section: root.navigationSection
                navPanel.order: 2
                navPanel.accessible.name: title

                model: [
                    {
                        label: qsTrc("playback", "Default"),
                        value: PlaybackMeterStyle.Default
                    },
                    {
                        label: qsTrc("playback", "RMS"),
                        value: PlaybackMeterStyle.RMS
                    },
                    {
                        label: qsTrc("playback", "Gradient"),
                        value: PlaybackMeterStyle.Gradient
                    }
                ]

                onValueChangeRequested: function (value) {
                    root.model.meterStyle = value
                }
            }

            StyledGroupBox {
                Layout.fillWidth: true
                Layout.preferredWidth: 0 // Force both to start with the same width
                Layout.preferredHeight: 120

                title: qsTrc("Playback", "Meter type")

                titleSpacing: 4

                value: root.model.meterType

                navPanel.name: "PlaybackMeterType"
                navPanel.section: root.navigationSection
                navPanel.order: 3
                navPanel.accessible.name: title

                model: [
                    {
                        label: qsTrc("playback", "Logarithmic (dB)"),
                        value: PlaybackMeterType.DbLog
                    },
                    {
                        label: qsTrc("playback", "Linear (dB)"),
                        value: PlaybackMeterType.DbLinear
                    },
                    {
                        label: qsTrc("playback", "Linear (amp)"),
                        value: PlaybackMeterType.Linear
                    }
                ]

                onValueChangeRequested: function (value) {
                    root.model.meterType = value
                }
            }
        }

        ColumnLayout {
            id: dbRangeSection

            Layout.fillWidth: true
            Layout.preferredHeight: 50

            spacing: 6

            NavigationPanel {
                id: dbRangeNavPanel

                name: "PlaybackMeterDbRange"
                section: root.navigationSection
                enabled: root.isOpened
                order: 4

                accessible.name: dbRangeLabel.text
            }

            StyledTextLabel {
                id: dbRangeLabel

                text: qsTrc("playback", "dB range")
                horizontalAlignment: Text.AlignLeft
                wrapMode: Text.WordWrap
            }

            DropdownWithTitle {
                id: dbRangeDropdown

                Layout.fillWidth: true
                Layout.preferredHeight: 28

                enabled: root.model.meterType !== PlaybackMeterType.Linear

                allowOptionToggle: false
                dropdownAccessibleName: dbRangeLabel.text

                current: root.model.description(root.model.meterDbRange)
                model: root.model.dbRangeList.map(function (range) {
                    return {
                        id: range,
                        title: root.model.description(range)
                    }
                })

                navigation.name: "DbRange"
                navigation.panel: dbRangeNavPanel
                navigation.order: 1

                onHandleMenuItem: function (itemId) {
                    root.model.meterDbRange = itemId
                }
            }
        }
    }
}
