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

    title: qsTrc("export", "Metadata editor")

    contentWidth: 880
    contentHeight: 625

    modal: true
    alwaysOnTop: true

    MetadataModel {
        id: metadataModel
    }

    Component.onCompleted: {
        metadataModel.load()
    }

    ColumnLayout {
        id: mainColumn

        spacing: 0

        RowLayout {
            id: topButtonsBar

            Layout.fillHeight: true
            Layout.fillWidth: true

            Layout.leftMargin: 16
            Layout.rightMargin: 12
            Layout.topMargin: 10
            Layout.bottomMargin: 10

            spacing: 6

            StyledTextLabel {
                id: title

                text: qsTrc("export", "Metadata editor")

                font.pixelSize: 18
            }

            Item {
                // spacer
                Layout.fillHeight: true
                Layout.fillWidth: true
            }

            FlatButton {
                Layout.preferredWidth: 106

                text: qsTrc("export", "Load template")

                onClicked: {
                    metadataModel.loadTemplate()
                }
            }

            FlatButton {
                Layout.preferredWidth: 105

                text: qsTrc("export", "Save template")

                onClicked: {
                    metadataModel.saveTemplate()
                }
            }

            FlatButton {
                Layout.preferredWidth: 103

                text: qsTrc("export", "Set as default")

                onClicked: {
                    metadataModel.setAsDefault()
                }
            }

            SeparatorLine {
                orientation: Qt.Vertical
            }

            FlatButton {
                Layout.preferredWidth: 61

                enabled: !metadataModel.isStandardTag(tagView.currentIndex)

                text: qsTrc("export", "Delete")

                onClicked: {
                    metadataModel.deleteTag(tagView.currentIndex)
                }
            }

            FlatButton {
                Layout.preferredWidth: 69

                text: qsTrc("export", "Add tag")

                onClicked: {
                    metadataModel.addTag()

                    // highlight right away
                    tagView.currentIndex = metadataModel.rowCount() - 1
                }
            }
        }

        SeparatorLine {}

        ColumnLayout {
            width: root.contentWidth
            height: 527

            spacing: 0

            Rectangle {
                id: header

                anchors.top: parent.top
                anchors.left: parent.left
                anchors.right: parent.right
                height: 38

                color: ui.theme.backgroundSecondaryColor

                RowLayout {
                    anchors.fill: parent

                    StyledTextLabel {
                        Layout.preferredWidth: 120
                        Layout.preferredHeight: header.height
                        Layout.leftMargin: 16

                        horizontalAlignment: Text.AlignLeft

                        text: "Tag"

                    }

                    SeparatorLine {
                        orientation: Qt.Vertical
                    }

                    StyledTextLabel {
                        Layout.preferredHeight: header.height
                        Layout.fillWidth: true
                        Layout.leftMargin: 16

                        horizontalAlignment: Text.AlignLeft

                        text: "Value"
                    }
                }
            }

            SeparatorLine {}

            StyledListView {
                id: tagView

                anchors.top: header.bottom
                anchors.left: parent.left
                anchors.leftMargin: background.border.width
                anchors.right: parent.right
                anchors.rightMargin: background.border.width
                anchors.bottom: parent.bottom
                anchors.bottomMargin: background.border.width

                model: metadataModel

                delegate: ListItemBlank {
                    height: 37
                    width: root.contentWidth

                    mouseArea.hoverEnabled: false
                    hoverHitColor: "transparent"
                    isSelected: ListView.isCurrentItem

                    onDoubleClicked: (mouse) => {
                        if (mouse.x > 142) {
                            valueEditLoader.edit(valueLabel.text)
                            return
                        }

                        if (metadataModel.isStandardTag(tagView.currentIndex)) {
                            return
                        }

                        tagEditLoader.edit(tagLabel.text)
                    }

                    onClicked: {
                        ListView.view.currentIndex = index
                    }

                    ColumnLayout {
                        width: parent.width

                        spacing: 0

                        RowLayout {
                            id: row

                            height: 37
                            Layout.fillWidth: true

                            Item {
                                Layout.preferredWidth: 120
                                Layout.preferredHeight: row.height
                                Layout.leftMargin: 16

                                StyledTextLabel {
                                    id: tagLabel

                                    anchors.fill: parent
                                    horizontalAlignment: Text.AlignLeft

                                    visible: !tagEditLoader.isEditState

                                    text: model.tag
                                }

                                Loader {
                                    id: tagEditLoader

                                    anchors.fill: tagLabel

                                    property bool isEditState: false
                                    sourceComponent: tagEditLoader.isEditState ? tagEditComp : null

                                    function edit(text) {
                                        tagEditLoader.isEditState = true
                                        tagEditLoader.item.currentText = text
                                        tagEditLoader.item.newTag = text
                                        tagEditLoader.item.visible = true
                                        tagEditLoader.item.ensureActiveFocus()
                                    }
                                }

                                Component {
                                    id: tagEditComp

                                    TextInputField {
                                        id: tagEdit

                                        property string newTag: ""

                                        anchors.fill: parent
                                        background.color: "transparent"
                                        background.border.width: 0
                                        inputField.color: tagLabel.color
                                        textSidePadding: 0
                                        visible: false

                                        onTextChanged: function (text) {
                                            tagEdit.newTag = text
                                        }

                                        onAccepted: {
                                            metadataModel.renameTag(index, tagEdit.newTag)
                                            tagEditLoader.isEditState = false
                                        }

                                        onEscaped: {
                                            tagEditLoader.isEditState = false
                                        }

                                        onFocusChanged: {
                                            if (!tagEdit.focus) {
                                                tagEdit.visible = false
                                                tagEdit.accepted()
                                            }
                                        }
                                    }
                                }
                            }

                            SeparatorLine {
                                orientation: Qt.Vertical
                            }

                            Item {
                                Layout.preferredWidth: 120
                                Layout.preferredHeight: row.height
                                Layout.leftMargin: 16

                                StyledTextLabel {
                                    id: valueLabel

                                    anchors.fill: parent
                                    horizontalAlignment: Text.AlignLeft

                                    visible: !valueEditLoader.isEditState

                                    text: model.value
                                }

                                Loader {
                                    id: valueEditLoader

                                    anchors.fill: valueLabel

                                    property bool isEditState: false
                                    sourceComponent: valueEditLoader.isEditState ? valueEditComp : null

                                    function edit(text) {
                                        valueEditLoader.isEditState = true
                                        valueEditLoader.item.currentText = text
                                        valueEditLoader.item.newValue = text
                                        valueEditLoader.item.visible = true
                                        valueEditLoader.item.ensureActiveFocus()
                                    }
                                }

                                Component {
                                    id: valueEditComp

                                    TextInputField {
                                        id: valueEdit

                                        property string newValue: ""

                                        anchors.fill: parent
                                        background.color: "transparent"
                                        background.border.width: 0
                                        inputField.color: valueLabel.color
                                        textSidePadding: 0
                                        visible: false

                                        onTextChanged: function (text) {
                                            valueEdit.newValue = text
                                        }

                                        onAccepted: {
                                            metadataModel.setTagValue(index, valueEdit.newValue)
                                            valueEditLoader.isEditState = false
                                        }

                                        onEscaped: {
                                            valueEditLoader.isEditState = false
                                        }

                                        onFocusChanged: {
                                            if (!valueEdit.focus) {
                                                valueEdit.visible = false
                                                valueEdit.accepted()
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        SeparatorLine {
                            Layout.fillWidth: true
                        }
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

                text: qsTrc("global", "Close")
                buttonRole: ButtonBoxModel.RejectRole
                buttonId: ButtonBoxModel.Cancel

                onClicked: root.reject()
            }

            FlatButton {
                id: okBtn

                minWidth: 80

                text: qsTrc("global", "Apply")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.Apply
                accentButton: true

                onClicked: {
                    root.accept()
                    metadataModel.apply()
                }
            }
        }
    }
}
