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
    required property int instanceId
    property string effectState: ""

    // out
    property string title: prv.viewModel.title
    property bool isPreviewing: prv.viewModel.isPreviewing

    implicitWidth: textItem.width
    implicitHeight: textItem.height
    color: ui.theme.backgroundPrimaryColor

    QtObject {
        id: prv
        property var viewModel: {
            var viewModel = Lv2ViewModelFactory.createModel(root, root.instanceId, root.effectState)
            viewModel.onExternalUiClosed.connect(function () {
                window.close()
            })
            return viewModel
        }
    }

    Component.onCompleted: {
        prv.viewModel.init()
        Qt.callLater(manageMenuModel.load)
    }

    Component.onDestruction: {
        prv.viewModel.deinit()
    }

    function startPreview() {
        prv.viewModel.startPreview()
    }

    function stopPreview() {
        prv.viewModel.stopPreview()
    }

    function manage(parent) {
        var px = parent.x
        var py = parent.y + parent.height
        var pos = mapFromItem(parent, px, py)

        menuLoader.show(pos, manageMenuModel)
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

    Text {
        id: textItem
        visible: prv.viewModel.unsupportedUiReason.length > 0
        width: visible ? 300 : 0
        height: visible ? 100 : 0
        anchors.centerIn: parent
        text: "No available UI:\n" + prv.viewModel.unsupportedUiReason + "\n(Plain UI not implemented yet)"
        color: ui.theme.fontPrimaryColor
        verticalAlignment: Text.AlignVCenter
        horizontalAlignment: Text.AlignHCenter
        wrapMode: Text.WordWrap
    }
}
