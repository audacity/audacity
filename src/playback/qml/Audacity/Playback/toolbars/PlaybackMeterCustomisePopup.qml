/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15
import QtQuick.Controls 2.15

import Muse.UiComponents 1.0
import Muse.Ui 1.0

import Audacity.Playback 1.0

StyledPopupView {
    id: root

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

            backgroundColor: ui.theme.backgroundSecondaryColor

            value: root.model.meterPosition

            model: [
                {label : qsTrc("playback","Top bar (horizontal)"), value: PlaybackMeterPosition.TopBar},
                {label : qsTrc("playback","Side bar (vertical)"), value: PlaybackMeterPosition.SideBar}
            ]

            onValueChangeRequested: function(value) {
                root.model.meterPosition = value
            }
        }

        RowLayout {
            Layout.fillWidth: true
            Layout.preferredHeight: 120
            
            spacing: 12

            StyledGroupBox {
                Layout.fillWidth: true
                Layout.preferredHeight: 120

                title: qsTrc("Playback", "Meter style")

                titleSpacing: 4

                backgroundColor: ui.theme.backgroundSecondaryColor

                value: root.model.meterStyle

                model: [
                    {label : qsTrc("playback","Default"), value: PlaybackMeterStyle.Default},
                    {label : qsTrc("playback","RMS"), value: PlaybackMeterStyle.RMS},
                    {label : qsTrc("playback","Gradient"), value: PlaybackMeterStyle.Gradient}
                ]

                onValueChangeRequested: function(value) {
                    root.model.meterStyle = value
                }
            }

            StyledGroupBox {
                Layout.fillWidth: true
                Layout.preferredHeight: 120

                title: qsTrc("Playback", "Meter type")

                titleSpacing: 4

                backgroundColor: ui.theme.backgroundSecondaryColor

                value: root.model.meterType

                model: [
                    {label : qsTrc("playback","Logarithmic (dB)"), value: PlaybackMeterType.DbLog},
                    {label : qsTrc("playback","Linear (dB)"), value: PlaybackMeterType.DbLinear},
                    {label : qsTrc("playback","Linear (amp)"), value: PlaybackMeterType.Linear}
                ]

                onValueChangeRequested: function(value) {
                    root.model.meterType = value
                }
            }
        }

        ColumnLayout {
            id: dbRangeSection

            Layout.fillWidth: true
            Layout.preferredHeight: 50

            spacing: 6

            StyledTextLabel {
                text: qsTrc("playback", "dB range")
                horizontalAlignment: Text.AlignLeft
                wrapMode: Text.WordWrap
            }

            Item {
                id: dropdown

                Layout.fillWidth: true
                Layout.preferredHeight: 28

                enabled: root.model.meterType !== PlaybackMeterType.Linear

                function openMenu() {
                    let rangeList = root.model.dbRangeList.map(function(range) {
                        return {
                            id: range,
                            title: root.model.description(range)
                        };
                    });
                    menuLoader.toggleOpened(rangeList)
                }

                Rectangle {
                    id: backgroundItem
                    anchors.fill: parent
 
                    color: ui.theme.textFieldColor
                    border.color: ui.theme.strokeColor
                    border.width: Math.max(ui.theme.borderWidth, 1)
                    radius: 3
                }

                StyledTextLabel {
                    id: labelItem

                    anchors.left: parent.left
                    anchors.leftMargin: 12
                    anchors.right: dropIconItem.left
                    anchors.verticalCenter: parent.verticalCenter

                    text: model.description(root.model.meterDbRange)
                    horizontalAlignment: Text.AlignLeft
                    wrapMode: Text.Wrap
                    maximumLineCount: 1
                }

                MouseArea {
                    id: mouseAreaItem
                    anchors.fill: parent
                    hoverEnabled: dropdown.enabled

                    onClicked: {
                        dropdown.openMenu()
                    }

                    onPressed: {
                        ui.tooltip.hide(dropdown, true)
                    }

                    onContainsMouseChanged: {
                        if (!labelItem.truncated || menuLoader.isMenuOpened) {
                            return
                        }

                        if (mouseAreaItem.containsMouse) {
                            ui.tooltip.show(dropdown, labelItem.text)
                        } else {
                            ui.tooltip.hide(dropdown)
                        }
                    }
                }

                StyledIconLabel {
                    id: dropIconItem
                    anchors.verticalCenter: parent.verticalCenter
                    anchors.right: parent.right
                    anchors.rightMargin: 8

                    iconCode: IconCode.SMALL_ARROW_DOWN
                }

                states: [
                    State {
                        name: "HOVERED"
                        when: mouseAreaItem.containsMouse && !mouseAreaItem.pressed
                        PropertyChanges { target: backgroundItem; border.color: Utils.colorWithAlpha(ui.theme.accentColor, 0.6) }
                    },

                    State {
                        name: "OPENED"
                        when: menuLoader.isMenuOpened
                        PropertyChanges { target: backgroundItem; border.color: ui.theme.accentColor; width: 336 }
                    }
                ]

                StyledMenuLoader {
                    id: menuLoader

                    anchors.top: parent.top

                    onHandleMenuItem: function(itemId) {
                        root.model.meterDbRange = itemId
                    }
                }
            }
        }
    }
}
