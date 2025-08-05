/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Audacity.BuiltinEffects

Rectangle {

    id: root

    property string instanceId: ""
    property var dialogView: null

    property string title: builder.contentItem ? builder.contentItem.title : ""
    property bool isApplyAllowed: builder.contentItem ? builder.contentItem.isApplyAllowed : false
    property bool usesPresets: builder.contentItem ? builder.contentItem.usesPresets : false

    signal closeRequested()

    color: ui.theme.backgroundPrimaryColor

    implicitWidth: builder.contentItem ? builder.contentItem.implicitWidth : 450
    implicitHeight: builder.contentItem ? builder.contentItem.implicitHeight : 200

    width: implicitWidth
    height: implicitHeight

    Component.onCompleted: {
        builder.load(root.instanceId, root, dialogView)
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

    BuiltinEffectViewLoader {
        id: builder

        onCloseRequested: root.closeRequested()
    }
}
