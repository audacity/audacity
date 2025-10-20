/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Audacity.BuiltinEffects

Rectangle {
    id: root

    property string instanceId: ""
    property var dialogView: null
    required property bool usedDestructively

    property string title: builder.contentItem ? builder.contentItem.title : ""
    property bool isApplyAllowed: builder.contentItem ? builder.contentItem.isApplyAllowed : false
    property bool isPreviewing: builder.contentItem ? builder.contentItem.isPreviewing : false
    property bool usesPresets: builder.contentItem ? builder.contentItem.usesPresets : false

    signal closeRequested

    color: ui.theme.backgroundPrimaryColor

    implicitWidth: builder.contentItem ? builder.contentItem.implicitWidth : 450
    implicitHeight: builder.contentItem ? builder.contentItem.implicitHeight : 200

    width: implicitWidth
    height: implicitHeight

    Component.onCompleted: {
        builder.load(root.instanceId, root, dialogView, usedDestructively)
        builder.contentItem.init()
    }

    function manage(parent) {
        if (builder.contentItem) {
            builder.contentItem.manage(parent)
        }
    }

    function togglePreview() {
        if (builder.contentItem) {
            builder.contentItem.togglePreview()
        }
    }

    BuiltinEffectViewLoader {
        id: builder

        onCloseRequested: root.closeRequested()
    }
}
