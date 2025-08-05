/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects

RowLayout {
    id: root

    property var instanceId
    property int navigationOrder: 0
    property var navigationPanel: null

    spacing: 4

    function manage(button) {
        var pos = Qt.point(button.x, button.y + button.height)
        menuLoader.show(pos, manageMenuModel)
    }

    Component.onCompleted: {
        Qt.callLater(manageMenuModel.load)
    }

    EffectManageMenu {
        id: manageMenuModel
        instanceId: root.instanceId
    }

    ContextMenuLoader {
        id: menuLoader

        onHandleMenuItem: function (itemId) {
            manageMenuModel.handleMenuItem(itemId)
        }
    }

    StyledDropdown {
        id: presetSelector

        navigation.panel: root.navigationPanel
        navigation.order: root.navigationOrder
        navigation.name: "preset dropdown"

        Layout.fillWidth: true
        background.color: ui.theme.backgroundPrimaryColor
        background.border.width: 1
        itemColor: "transparent"

        textRole: "name"
        valueRole: "id"

        enabled: manageMenuModel.enabled

        indeterminateText: qsTrc("effects", "Select preset")

        model: manageMenuModel.presets

        onActivated: function (index, value) {
            manageMenuModel.preset = value
            currentIndex = manageMenuModel.presets.findIndex(
                        preset => preset.id === value)
        }
    }

    FlatButton {
        id: saveBtn

        navigation.panel: root.navigationPanel
        navigation.order: presetSelector.navigation.order + 1
        navigation.name: "save preset btn"

        Layout.alignment: Qt.AlignVCenter
        icon: IconCode.SAVE

        onClicked: {
            manageMenuModel.savePresetAs()
        }
    }

    FlatButton {
        id: resetBtn

        navigation.panel: root.navigationPanel
        navigation.order: saveBtn.navigation.order + 1
        navigation.name: "reset preset btn"

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

        navigation.panel: root.navigationPanel
        navigation.order: resetBtn.navigation.order + 1
        navigation.name: "manage preset btn"

        Layout.alignment: Qt.AlignVCenter

        icon: IconCode.MENU_THREE_DOTS

        onClicked: {
            root.manage(manageButton)
        }
    }
}
