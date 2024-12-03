import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects

Rectangle {

    id: root

    property var instanceId: null

    property AbstractEffectModel model: null

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
}
