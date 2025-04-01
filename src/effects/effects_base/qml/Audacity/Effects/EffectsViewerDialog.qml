/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects
import Audacity.Vst

StyledDialogView {
    id: root

    property var instanceId
    property bool isVst: false

    property alias viewer: viewerLoader.item
    property bool isApplyAllowed: isVst || (viewer && viewer.isApplyAllowed)

    title: viewer ? viewer.title : ""

    contentWidth: Math.max(viewerLoader.width, 300)
    contentHeight: presetRow.height + separator.height + viewerLoader.height + btnBarLoader.height + margins * 3

    margins: 16

    EffectManageMenu {
        id: manageMenuModel
        instanceId: root.instanceId
    }

    onWindowChanged: {
        // Wait until the window is set: VstView needs it for intialization
        viewerLoader.sourceComponent = isVst ? vstViewerComp : builtinViewerComp
    }

    Component.onCompleted: {
        // Delay loading of ButtonBox because it needs to know the final width before executing its layout
        // (which it only does once)
        btnBarLoader.sourceComponent = bboxComponent
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

    Column {
        id: column
        anchors.fill: parent

        spacing: 16

        RowLayout {
            id: presetRow
            spacing: 4
            anchors.left: parent.left
            anchors.right: parent.right

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

        SeparatorLine {
            id: separator
        }

        Component {
            id: builtinViewerComp
            EffectsViewer {
                instanceId: root.instanceId
            }
        }

        Component {
            id: vstViewerComp
            VstViewer {
                instanceId: root.instanceId
                height: implicitHeight
                x: root.margins
                y: root.margins * 3 + presetRow.height + separator.height
            }
        }

        Loader {
            id: viewerLoader
        }

        Component {
            id: bboxComponent
            ButtonBox {
                id: bbox

                width: root.contentWidth

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

                FlatButton {
                    id: previewBtn
                    text: qsTrc("effects", "Preview")
                    buttonRole: ButtonBoxModel.CustomRole
                    buttonId: ButtonBoxModel.CustomButton + 2
                    isLeftSide: true
                    minWidth: 80
                    onClicked: viewer.preview()
                    enabled: root.isApplyAllowed
                }

                FlatButton {
                    id: cancelBtn
                    text: qsTrc("global", "Cancel")
                    buttonRole: ButtonBoxModel.RejectRole
                    buttonId: ButtonBoxModel.Cancel
                    minWidth: 80
                    onClicked: root.reject()
                }

                FlatButton {
                    id: okBtn
                    text: qsTrc("global", "Apply")
                    buttonRole: ButtonBoxModel.AcceptRole
                    buttonId: ButtonBoxModel.Apply
                    minWidth: 80
                    accentButton: true
                    onClicked: root.accept()
                    enabled: root.isApplyAllowed
                }
            }
        }

        Loader {
            id: btnBarLoader
        }
    }
}
