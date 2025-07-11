/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.Effects 1.0
import Audacity.AudioUnit 1.0

Rectangle {

    id: root

    // in
    property alias instanceId: view.instanceId
    property alias sidePadding: view.sidePadding
    property alias topPadding: view.topPadding
    property alias bottomPadding: view.bottomPadding
    property alias minimumWidth: view.minimumWidth

    // out
    property alias title: model.title

    color: ui.theme.backgroundPrimaryColor

    width: implicitWidth
    height: implicitHeight

    implicitWidth: view.implicitWidth
    implicitHeight: view.implicitHeight


    Component.onCompleted: {
        model.init()
        view.init()
        Qt.callLater(manageMenuModel.load)
    }

    Component.onDestruction: {
        model.deinit();
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

    AudioUnitViewModel {
        id: model
        instanceId: view.instanceId
    }

    AudioUnitView {
        id: view
    }
}
