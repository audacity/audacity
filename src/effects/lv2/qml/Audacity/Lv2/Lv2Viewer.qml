/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick

import Audacity.Lv2

Rectangle {

    id: root

    property string instanceId: ""

    property string title: model.contentItem ? model.contentItem.title : ""
    property bool isApplyAllowed: model.contentItem ? model.contentItem.isApplyAllowed : false

    signal closeRequested()

    color: ui.theme.backgroundPrimaryColor

    implicitWidth: model.contentItem ? model.contentItem.implicitWidth : 450
    implicitHeight: model.contentItem ? model.contentItem.implicitHeight : 200

    width: implicitWidth
    height: implicitHeight

    Component.onCompleted: {
        model.load(root.instanceId, root)
    }

    function manage(parent) {
        if (model.contentItem) {
            model.contentItem.manage(parent)
        }
    }

    function preview() {
        if (model.contentItem) {
            model.contentItem.preview()
        }
    }

    Lv2ViewModel {
        id: model

        onCloseRequested: root.closeRequested()
    }
}
