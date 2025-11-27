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
    required property int instanceId
    property alias sidePadding: view.sidePadding
    property alias topPadding: view.topPadding
    property alias bottomPadding: view.bottomPadding
    property alias minimumWidth: view.minimumWidth

    // out
    property bool title: model.title
    property bool isPreviewing: model.isPreviewing

    readonly property var model: AudioUnitViewModelFactory.createModel(root, root.instanceId)

    property alias view: view

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
        model.deinit()
    }

    // Reload the view when UI mode changes
    Connections {
        target: manageMenuModel
        function onUseVendorUIChanged() {
            view.reload()
        }
    }

    function startPreview() {
        model.startPreview()
    }

    function stopPreview() {
        model.stopPreview()
    }

    function reload() {
        view.reload()
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

        onHandleMenuItem: function (itemId) {
            manageMenuModel.handleMenuItem(itemId)
        }
    }

    AudioUnitView {
        id: view
        instanceId: model.instanceId
    }
}
