/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick

import Muse.UiComponents
import Muse.Ui

import Audacity.Effects
import Audacity.Lv2

Rectangle {

    id: root

    // in
    property alias instanceId: model.instanceId
    property alias effectState: model.effectState
    property alias title: model.title

    implicitWidth: textItem.width
    implicitHeight: textItem.height
    color: ui.theme.backgroundPrimaryColor

    Component.onCompleted: {
        model.init()
        Qt.callLater(manageMenuModel.load)
    }

    Component.onDestruction: {
        model.deinit()
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
        instanceId: model.instanceId
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

    Text {
        id: textItem
        visible: model.unsupportedUiReason.length > 0
        width: visible ? 300 : 0
        height: visible ? 100 : 0
        anchors.centerIn: parent
        text: "No available UI:\n" + model.unsupportedUiReason + "\n(Plain UI not implemented yet)"
        color: ui.theme.fontPrimaryColor
        verticalAlignment: Text.AlignVCenter
        horizontalAlignment: Text.AlignHCenter
        wrapMode: Text.WordWrap
    }
}
