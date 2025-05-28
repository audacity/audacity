/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick

import Audacity.Lv2

Rectangle {

    id: root

    property string instanceId: ""

    property string title: builder.contentItem ? builder.contentItem.title : ""
    property bool isApplyAllowed: builder.contentItem ? builder.contentItem.isApplyAllowed : false

    signal closeRequested()

    color: ui.theme.backgroundPrimaryColor

    implicitWidth: builder.contentItem ? builder.contentItem.implicitWidth : 450
    implicitHeight: builder.contentItem ? builder.contentItem.implicitHeight : 200

    width: implicitWidth
    height: implicitHeight

    Component.onCompleted: {
        builder.load(root.instanceId, root)
    }

    function manage(parent) {
        if (builder.contentItem) {
            builder.contentItem.manage(parent)
        }
    }

    function preview() {
        if (builder.contentItem) {
            builder.contentItem.preview()
        }
    }

    Lv2ViewLoader {
        id: builder

        onCloseRequested: root.closeRequested()
    }
}
