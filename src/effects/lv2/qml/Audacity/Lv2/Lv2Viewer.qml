/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick

import Muse.UiComponents

import Audacity.Effects
import Audacity.Lv2

Rectangle {

    id: root

    // in
    property alias instanceId: model.instanceId

    // out
    property alias title: model.title

    color: ui.theme.backgroundPrimaryColor

    implicitWidth: view.implicitWidth
    implicitHeight: view.implicitHeight

    Component.onCompleted: {
        model.init()
        view.init()
        Qt.callLater(manageMenuModel.load)
    }

    function preview() {
       model.preview()
    }

    function manage(parent) {
        var px = parent.x
        var py = parent.y + parent.height
        var pos = mapFromItem(parent, px, py)

        menuLoader.show(pos, manageMenuModel)
    }

    EffectManageMenu {
        id: manageMenuModel
        instanceId: view.instanceId
    }

    ContextMenuLoader {
        id: menuLoader

        onHandleMenuItem: function(itemId) {
            manageMenuModel.handleMenuItem(itemId)
        }
    }

    Lv2ViewModel {
        id: model
        onExternalUiClosed: {
            window.close()
        }
    }
}
