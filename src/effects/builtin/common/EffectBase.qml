import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects

Rectangle {

    id: root

    property var instanceId: null

    property AbstractEffectModel model: null

    default property alias content: effectSettings.data

    color: ui.theme.backgroundPrimaryColor

    function manage(parent) {
        var px = parent.x
        var py = parent.y + parent.height
        var pos = mapFromItem(parent, px, py)

        menuLoader.show(pos, manageMenuModel)
    }

    function preview() {
        root.model.preview()
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

        onHandleMenuItem: function(itemId) {
            manageMenuModel.handleMenuItem(itemId)
        }
    }

    ColumnLayout {
        anchors.fill: parent

        spacing: 16

        RowLayout {
            spacing: 4

            StyledDropdown {
                Layout.fillWidth: true
                indeterminateText: "Default preset"
            }

            FlatButton {
                Layout.alignment: Qt.AlignVCenter
                icon: IconCode.SAVE
            }

            FlatButton {
                Layout.alignment: Qt.AlignVCenter
                icon: IconCode.UNDO
            }

            FlatButton {
                Layout.alignment: Qt.AlignVCenter
                icon: IconCode.MENU_THREE_DOTS
            }
        }

        SeparatorLine {

        }

        Item {
            id: effectSettings
            Layout.fillWidth: true
            Layout.fillHeight: true
        }
    }
}
