/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Audacity.Effects

Rectangle {

    id: root

    property string type: ""
    property string instanceId: ""

    signal closeRequested()

    color: ui.theme.backgroundPrimaryColor

    property string title: builder.contentItem ? builder.contentItem.title : ""
    implicitWidth: builder.contentItem ? builder.contentItem.implicitWidth : 450
    implicitHeight: builder.contentItem ? builder.contentItem.implicitHeight : 200

    width: implicitWidth
    height: implicitHeight

    Component.onCompleted: {
        builder.load(root.type, root.instanceId, root)
    }

    EffectBuilder {
        id: builder

        onCloseRequested: root.closeRequested()
    }
}
