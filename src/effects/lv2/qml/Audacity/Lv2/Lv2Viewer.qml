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

    signal vendorUiFailed

    implicitWidth: textItem.width
    implicitHeight: textItem.height
    color: ui.theme.backgroundPrimaryColor

    QtObject {
        id: prv

        property bool vendorUiFailed: false
        property AbstractMenuModel activeMenuModel: null
        property var viewModel: {
            var viewModel = Lv2ViewModelFactory.createModel(root, root.instanceId, root.effectState)
            viewModel.onExternalUiClosed.connect(function () {
                window.close()
            })
            viewModel.onVendorUiFailed.connect(function () {
                prv.vendorUiFailed = true
                root.vendorUiFailed()
            })
            return viewModel
        }
    }

    Component.onCompleted: {
        prv.viewModel.init()

        if (!prv.vendorUiFailed) {
            Qt.callLater(presetsBarModel.load)
        }
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

        prv.activeMenuModel = presetsBarModel.presetContextMenu()
        menuLoader.show(pos, prv.activeMenuModel)
    }

    EffectPresetsBarModel {
        id: presetsBarModel
        instanceId: root.instanceId
    }

    ContextMenuLoader {
        id: menuLoader

        onHandleMenuItem: function (itemId) {
            if (prv.activeMenuModel) {
                prv.activeMenuModel.handleMenuItem(itemId)
            }
        }
    }

    Text {
        id: textItem
        visible: prv.viewModel.unsupportedUiReason.length > 0
        width: visible ? 300 : 0
        height: visible ? 100 : 0
        anchors.centerIn: parent
        text: qsTrc("effects/lv2", "No available UI:\n%1").arg(prv.viewModel.unsupportedUiReason)
        color: ui.theme.fontPrimaryColor
        verticalAlignment: Text.AlignVCenter
        horizontalAlignment: Text.AlignHCenter
        wrapMode: Text.WordWrap
    }
}
