/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
 *
 * Copyright (C) 2024 Audacity BVBA and others
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
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Muse.GraphicalEffects 1.0
import Muse.Cloud 1.0

StyledDialogView {
    id: root

    property bool isPublishShare: false
    property string name
    property int visibility: CloudVisibility.Private
    property string existingProjectOrAudioUrl
    property bool replaceExisting: true
    property string cloudCode: ""

    contentWidth: contentItem.implicitWidth
    contentHeight: contentItem.implicitHeight

    margins: 20

    function done(response, data = {}) {
        let value = Object.assign(({ response: response }), data)

        root.ret = {
            errcode: 0,
            value: value
        }

        root.hide()
    }

    Item {
        id: contentItem

        property var cloudInfo: null
        property var dialogText: null
        property var visibilityModel: null

        implicitWidth: Math.max(420, contentColumn.implicitWidth)
        implicitHeight: contentColumn.implicitHeight

        ColumnLayout {
            id: contentColumn

            anchors.fill: parent

            spacing: 20

            ColumnLayout {
                id: headerColumn

                spacing: root.isPublishShare ? 16 : 0

                Item {
                    id: cloudImages

                    width: contentItem.width
                    height: root.isPublishShare ? childrenRect.height : 0

                    Image {
                        id: cloudLogo

                        visible: false

                        anchors.verticalCenter: avatar.verticalCenter
                        anchors.left: parent.left

                        source: contentItem.cloudInfo.cloudLogoUrl
                        sourceSize.height: 20
                    }

                    EffectColorOverlay {
                        visible: root.isPublishShare

                        anchors.fill: cloudLogo

                        color: contentItem.cloudInfo.cloudLogoColor

                        source: cloudLogo
                    }

                    AccountAvatar {
                        id: avatar

                        anchors.right: parent.right

                        side: 38
                        url: Boolean(contentItem.cloudInfo) ? contentItem.cloudInfo.userAvatarUrl : null

                        CloudsModel {
                            id: cloudsModel

                            Component.onCompleted: {
                                load()

                                contentItem.cloudInfo = cloudsModel.cloudInfo(root.cloudCode)
                                contentItem.dialogText = cloudsModel.dialogText(root.cloudCode, existingProjectOrAudioUrl)
                                contentItem.visibilityModel = cloudsModel.visibilityModel(root.cloudCode)
                            }
                        }
                    }
                }

                StyledTextLabel {
                    id: titleLabel

                    text: root.isPublishShare && Boolean(contentItem.dialogText) ? contentItem.dialogText.titleText
                                                                                 : qsTrc("project/save", "Save to cloud")

                    font: ui.theme.largeBodyBoldFont
                    horizontalAlignment: Text.AlignLeft
                }
            }

            ColumnLayout {
                id: optionsColumn
                spacing: 16

                NavigationPanel {
                    id: optionsNavPanel
                    name: "SaveToCloudOptions"
                    enabled: optionsColumn.enabled && optionsColumn.visible
                    direction: NavigationPanel.Vertical
                    section: root.navigationSection
                    order: 1
                    accessible.name: qsTrc("project/save", "Options")
                }

                ColumnLayout {
                    spacing: 8

                    StyledTextLabel {
                        Layout.fillWidth: true
                        text: qsTrc("project/save", "Name")
                        horizontalAlignment: Text.AlignLeft
                    }

                    TextInputField {
                        Layout.fillWidth: true
                        currentText: root.name

                        navigation.panel: optionsNavPanel
                        navigation.row: 1
                        accessible.name: titleLabel.text + ". " + qsTrc("project/save", "Name") + ": " + currentText

                        onTextChanged: function(newTextValue) {
                            root.name = newTextValue
                        }
                    }
                }

                ColumnLayout {
                    spacing: 8

                    StyledTextLabel {
                        Layout.fillWidth: true
                        //: visibility of a project on Audio.com: private, public or unlisted
                        text: qsTrc("project/cloud", "Visibility")
                        horizontalAlignment: Text.AlignLeft
                    }

                    StyledDropdown {
                        Layout.fillWidth: true

                        model: contentItem.visibilityModel

                        currentIndex: indexOfValue(root.visibility)

                        navigation.panel: optionsNavPanel
                        navigation.row: 2
                        navigation.accessible.name: qsTrc("project/cloud", "Visibility") + ": " + currentText

                        onActivated: function(index, value) {
                            root.visibility = value
                        }
                    }
                }

                RadioButtonGroup {
                    Layout.fillWidth: true

                    orientation: ListView.Vertical
                    spacing: 8

                    visible: root.isPublishShare && Boolean(root.existingProjectOrAudioUrl)

                    model: [
                        { text: Boolean(contentItem.dialogText) ? contentItem.dialogText.replaceButtonText
                                                                : qsTrc("project/save", "Replace existing"), value: true },
                        { text: Boolean(contentItem.dialogText) ? contentItem.dialogText.newButtonText
                                                                : qsTrc("project/save", "Create new"), value: false }
                    ]

                    delegate: RoundedRadioButton {
                        checked: modelData.value === root.replaceExisting
                        text: modelData.text

                        navigation.name: modelData.text
                        navigation.panel: optionsNavPanel
                        navigation.row: 3 + model.index

                        onToggled: {
                            root.replaceExisting = modelData.value
                        }
                    }
                }
            }

            ButtonBox {
                id: buttonBox

                Layout.fillWidth: true

                buttons: [ ButtonBoxModel.Cancel ]

                navigationPanel.section: root.navigationSection
                navigationPanel.order: 2

                FlatButton {
                    text: qsTrc("project/save", "Save to computer")
                    buttonRole: ButtonBoxModel.ApplyRole
                    buttonId: ButtonBoxModel.CustomButton + 1
                    visible: !root.isPublishShare

                    onClicked: {
                        root.done(SaveToCloudResponse.SaveLocallyInstead)
                    }
                }

                FlatButton {
                    text: root.isPublishShare && Boolean(contentItem.dialogText) ? contentItem.dialogText.saveButtonText
                                                                                 : qsTrc("project/save", "Save")

                    buttonRole: ButtonBoxModel.ApplyRole
                    buttonId: ButtonBoxModel.Save
                    enabled: Boolean(root.name)
                    accentButton: true

                    onClicked: {
                        root.done(SaveToCloudResponse.Ok, {
                                      name: root.name,
                                      visibility: root.visibility,
                                      replaceExisting: root.replaceExisting
                                  })
                    }
                }

                onStandardButtonClicked: function(buttonId) {
                    if (buttonId === ButtonBoxModel.Cancel) {
                        root.done(SaveToCloudResponse.Cancel)
                    }
                }
            }
        }
    }
}

