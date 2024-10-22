/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Audacity.Effects

Rectangle {

    id: root

    property string type: ""
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
        builder.load(root.type, root.instanceId, root)
    }

    function preview() {
        console.debug("[qml] EffectsViewer preview ")
        if (!builder.contentItem) {
            return;
        }

        builder.contentItem.preview()
    }

    EffectBuilder {
        id: builder

        onCloseRequested: root.closeRequested()
    }
}
