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

    required property var instanceId
    property bool destructiveMode: false
    property int navigationOrder: 0
    property var navigationPanel: null

    property var parentWindow: null

    // Expose the manage menu model so parent can listen to its signals
    property alias manageMenuModel: manageMenuModel

    property var activeMenuModel: null

    spacing: 4

    function presetIconCodeById(presetId) {
        const preset = manageMenuModel.presets.find(item => item.id === presetId)
        return preset && preset.iconCode ? preset.iconCode : IconCode.NONE
    }

    function presetIconCodeByName(name) {
        const preset = manageMenuModel.presets.find(item => item.name === name)
        return preset && preset.iconCode ? preset.iconCode : IconCode.NONE
    }

    function manage(button) {
        activeMenuModel = manageMenuModel.presetContextMenu()
        var pos = Qt.point(button.x, button.y + button.height)
        menuLoader.show(pos, activeMenuModel)
    }

    function save(button) {
        activeMenuModel = manageMenuModel.saveContextMenu()
        var pos = Qt.point(button.x, button.y + button.height)
        menuLoader.show(pos, activeMenuModel)
    }

    Component.onCompleted: {
        Qt.callLater(manageMenuModel.load)
    }

    EffectManageMenu {
        id: manageMenuModel
        instanceId: root.instanceId
        persistLastUsedPreset: root.destructiveMode
    }

    Connections {
        target: manageMenuModel

        function onPresetChanged() {
            presetSelector.currentIndex = manageMenuModel.presets.findIndex(preset => preset.id === manageMenuModel.preset)
        }

        function onPresetsChanged() {
            presetSelector.currentIndex = manageMenuModel.presets.findIndex(preset => preset.id === manageMenuModel.preset)
        }
    }

    ContextMenuLoader {
        id: menuLoader

        visible: false

        parentWindow: root.parentWindow

        onHandleMenuItem: function (itemId) {
            if (root.activeMenuModel) {
                root.activeMenuModel.handleMenuItem(itemId)
            }
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

        parentWindow: root.parentWindow
        enabled: manageMenuModel.enabled

        indeterminateText: qsTrc("effects", "Select preset")

        model: manageMenuModel.presets
        displayText: {
            const preset = manageMenuModel.presets.find(item => item.id === manageMenuModel.preset)
            return preset ? preset.name : indeterminateText
        }

        contentItem: RowLayout {
            property alias labelItem: textItem
            property alias text: textItem.text

            anchors.fill: parent
            anchors.leftMargin: 12
            anchors.rightMargin: 8
            spacing: 6

            StyledIconLabel {
                Layout.preferredWidth: ui.theme.iconsFont.pixelSize
                Layout.alignment: Qt.AlignVCenter
                readonly property int presetIconCode: root.presetIconCodeById(manageMenuModel.preset)
                iconCode: presetIconCode
            }

            StyledTextLabel {
                id: textItem

                Layout.fillWidth: true
                Layout.alignment: Qt.AlignVCenter
                horizontalAlignment: Text.AlignLeft
            }

            StyledIconLabel {
                Layout.alignment: Qt.AlignVCenter
                iconCode: IconCode.SMALL_ARROW_DOWN
            }
        }

        contentListItem: RowLayout {
            property alias text: textItem.text
            property alias truncated: textItem.truncated

            anchors.fill: parent
            anchors.leftMargin: 12
            spacing: 8

            StyledIconLabel {
                Layout.preferredWidth: ui.theme.iconsFont.pixelSize
                Layout.alignment: Qt.AlignVCenter
                readonly property int presetIconCode: root.presetIconCodeByName(textItem.text)
                iconCode: presetIconCode
            }

            StyledTextLabel {
                id: textItem

                Layout.fillWidth: true
                Layout.alignment: Qt.AlignVCenter
                horizontalAlignment: Text.AlignLeft
            }
        }

        onActivated: function (index, value) {
            manageMenuModel.preset = value
            currentIndex = manageMenuModel.presets.findIndex(preset => preset.id === value)
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
            root.save(saveBtn)
        }
    }

    FlatButton {
        id: resetBtn

        navigation.panel: root.navigationPanel
        navigation.order: saveBtn.navigation.order + 1
        navigation.name: "reset preset btn"

        Layout.alignment: Qt.AlignVCenter

        icon: IconCode.UNDO
        enabled: manageMenuModel.canResetPreset

        onClicked: {
            manageMenuModel.resetPreset()
        }
    }

    FlatButton {
        id: deleteBtn

        navigation.panel: root.navigationPanel
        navigation.order: resetBtn.navigation.order + 1
        navigation.name: "delete preset btn"

        Layout.alignment: Qt.AlignVCenter

        icon: IconCode.DELETE_TANK
        enabled: manageMenuModel.canDeletePreset

        onClicked: {
            manageMenuModel.deletePreset()
        }
    }

    FlatButton {
        id: manageButton

        navigation.panel: root.navigationPanel
        navigation.order: deleteBtn.navigation.order + 1
        navigation.name: "manage preset btn"

        Layout.alignment: Qt.AlignVCenter

        icon: IconCode.MENU_THREE_DOTS

        onClicked: {
            root.manage(manageButton)
        }
    }
}
