/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
import QtQuick 2.15

import Muse.Ui
import Muse.UiComponents

import Audacity.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("preferences", "Inputs and outputs")
    spacing: 16

    property int currentAudioApiIndex: -1
    property var audioApiList: null
    property var apiModel: null

    signal currentAudioApiIndexChangeRequested(int newIndex)

    Row {
        width: parent.width
        spacing: root.spacing

        Column {
            width: root.columnWidth
            spacing: root.spacing

            ComboBoxWithTitle {
                title: qsTrc("preferences", "Host")
                columnWidth: root.columnWidth

                currentIndex: apiModel.currentAudioApiIndex
                model: root.audioApiList

                navigation.name: "AudioApiBox"
                navigation.panel: root.navigation
                navigation.row: 1
                navigation.column: 0

                onValueEdited: function (newIndex, newValue) {
                    root.currentAudioApiIndexChangeRequested(newIndex)
                }
            }

            ComboBoxWithTitle {
                title: qsTrc("preferences", "Recording device")
                columnWidth: root.columnWidth

                currentIndex: apiModel.currentInputDeviceIndex
                model: apiModel.inputDeviceList

                navigation.name: "RecordingDeviceBox"
                navigation.panel: root.navigation
                navigation.row: 2
                navigation.column: 0

                onValueEdited: function (newIndex, newValue) {
                    apiModel.inputDeviceSelected(newIndex)
                }
            }
        }

        Column {
            width: root.columnWidth
            spacing: root.spacing

            ComboBoxWithTitle {
                title: qsTrc("preferences", "Playback device")
                columnWidth: root.columnWidth

                currentIndex: apiModel.currentOutputDeviceIndex
                model: apiModel.outputDeviceList

                navigation.name: "PlaybackDeviceBox"
                navigation.panel: root.navigation
                navigation.row: 1
                navigation.column: 1

                onValueEdited: function (newIndex, newValue) {
                    apiModel.outputDeviceSelected(newIndex)
                }
            }

            Column {
                width: root.columnWidth
                spacing: 6

                StyledTextLabel {
                    width: parent.width
                    text: qsTrc("preferences", "Recording channels")
                    horizontalAlignment: Text.AlignLeft
                }

                FlatButton {
                    id: recordingChannelsButton
                    width: parent.width
                    text: apiModel.inputChannelSelectionSummary
                    orientation: Qt.Horizontal

                    navigation.name: "RecordingChannelsBox"
                    navigation.panel: root.navigation
                    navigation.row: 2
                    navigation.column: 1
                    navigation.accessible.name: qsTrc("preferences", "Recording channels") + " " + text

                    onClicked: recordingChannelsPopup.toggleOpened()

                    StyledPopupView {
                        id: recordingChannelsPopup
                        objectName: "RecordingChannelsPopup"
                        contentWidth: recordingChannelsList.width
                        contentHeight: recordingChannelsList.height
                        navigationSection.name: objectName

                        NavigationPanel {
                            id: recordingChannelsNavigation
                            name: "RecordingChannelsPopup"
                            order: 1
                            enabled: recordingChannelsPopup.isOpened
                            direction: NavigationPanel.Vertical
                            section: recordingChannelsPopup.navigationSection
                        }

                        StyledListView {
                            id: recordingChannelsList
                            objectName: "RecordingChannelsList"
                            width: root.columnWidth
                            height: Math.min(contentHeight, 400)
                            clip: true
                            spacing: 8
                            model: RecordingChannelListModel {
                                groups: apiModel.inputChannelGroups
                            }

                            delegate: Item {
                                id: channelDelegate

                                required property var model
                                required property int index

                                readonly property bool hasSectionHeader: model.sectionStart

                                width: recordingChannelsList.width
                                height: channelCheckBox.implicitHeight + (hasSectionHeader ? sectionHeader.implicitHeight + 8 : 0)

                                StyledTextLabel {
                                    id: sectionHeader
                                    visible: hasSectionHeader
                                    anchors.top: parent.top
                                    anchors.left: parent.left
                                    anchors.leftMargin: 8
                                    text: channelDelegate.model.channelCount === 1 ? qsTrc("preferences", "Mono channels") : qsTrc("preferences", "Stereo channels")
                                    font: ui.theme.bodyBoldFont
                                    horizontalAlignment: Text.AlignLeft
                                }

                                CheckBox {
                                    id: channelCheckBox
                                    anchors.left: parent.left
                                    anchors.right: parent.right
                                    anchors.leftMargin: 8
                                    anchors.rightMargin: 8
                                    anchors.bottom: parent.bottom
                                    text: channelDelegate.model.title
                                    checked: channelDelegate.model.checked

                                    navigation.name: "RecordingChannel" + channelDelegate.model.title
                                    navigation.panel: recordingChannelsNavigation
                                    navigation.row: index
                                    navigation.column: 0
                                    navigation.onActiveChanged: {
                                        if (navigation.active) {
                                            recordingChannelsList.positionViewAtIndex(index, ListView.Contain)
                                        }
                                    }

                                    onClicked: apiModel.toggleInputChannelGroup(channelDelegate.model.firstChannel, channelDelegate.model.channelCount)
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
