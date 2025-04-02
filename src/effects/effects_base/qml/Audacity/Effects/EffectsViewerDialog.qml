/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects

StyledDialogView {
    id: root

    property alias instanceId: viewer.instanceId

    title: viewer.title

    contentWidth: Math.max(viewer.implicitWidth, bbox.implicitWidth)
    contentHeight: viewer.implicitHeight + bbox.implicitHeight + 16

    margins: 16

    EffectManageMenu {
        id: manageMenuModel
        instanceId: root.instanceId
    }

    Component.onCompleted: {
        Qt.callLater(manageMenuModel.load)
    }

    function manage(button) {
        var pos = Qt.point(button.x, button.y + button.height)
        menuLoader.show(pos, manageMenuModel)
    }

    ContextMenuLoader {
        id: menuLoader

        onHandleMenuItem: function (itemId) {
            manageMenuModel.handleMenuItem(itemId)
        }
    }

    ColumnLayout {
        anchors.fill: parent

        spacing: 16

        RowLayout {
            spacing: 4

            StyledDropdown {
                id: presetSelector

                Layout.fillWidth: true
                background.color: ui.theme.backgroundPrimaryColor
                background.border.width: 1
                itemColor: "transparent"

                textRole: "name"
                valueRole: "id"

                indeterminateText: qsTrc("effects", "Select preset")

                model: manageMenuModel.presets

                onActivated: function (index, value) {
                    manageMenuModel.preset = value
                    currentIndex = manageMenuModel.presets.findIndex(
                                preset => preset.id === value)
                }
            }

            FlatButton {
                Layout.alignment: Qt.AlignVCenter
                icon: IconCode.SAVE

                onClicked: {
                    manageMenuModel.savePresetAs()
                }
            }

            FlatButton {
                Layout.alignment: Qt.AlignVCenter

                icon: IconCode.UNDO

                onClicked: {
                    if (manageMenuModel.preset === "") {
                        manageMenuModel.preset = "default"
                        presetSelector.currentIndex = 0
                    }

                    manageMenuModel.resetPreset()
                }
            }

            FlatButton {
                id: manageButton

                Layout.alignment: Qt.AlignVCenter

                icon: IconCode.MENU_THREE_DOTS

                onClicked: {
                    root.manage(manageButton)
                }
            }
        }

        SeparatorLine {}

        EffectsViewer {
            id: viewer

            Layout.preferredWidth: parent.width

            onIsApplyAllowedChanged: {
                bbox.buttonById(ButtonBoxModel.Apply).enabled = isApplyAllowed
                bbox.buttonById(previewBtn.buttonId).enabled = isApplyAllowed
            }
        }
    }
    ButtonBox {
        id: bbox

        width: parent.width
        anchors.bottom: parent.bottom

        //! TODO Move function to ButtonBox (Muse framework)
        function buttonById(id) {
            for (var i = 0; i < bbox.count; i++) {
                var btn = bbox.itemAt(i)
                if (btn.buttonId === id) {
                    return btn
                }
            }

            return null
        }

        Component.onCompleted: {
            bbox.buttonById(ButtonBoxModel.Apply).enabled = false
        }

        FlatButton {
            id: previewBtn
            text: qsTrc("effects", "Preview")
            buttonRole: ButtonBoxModel.CustomRole
            buttonId: ButtonBoxModel.CustomButton + 2
            isLeftSide: true
            minWidth: 80
            onClicked: viewer.preview()
        }

        FlatButton {
            text: qsTrc("global", "Cancel")
            buttonRole: ButtonBoxModel.RejectRole
            buttonId: ButtonBoxModel.Cancel
            minWidth: 80
            onClicked: root.reject()
        }

        FlatButton {
            text: qsTrc("global", "Apply")
            buttonRole: ButtonBoxModel.AcceptRole
            buttonId: ButtonBoxModel.Apply
            minWidth: 80
            accentButton: true
            onClicked: root.accept()
        }
    }
}
