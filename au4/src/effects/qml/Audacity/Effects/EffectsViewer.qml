/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Audacity.Effects

Rectangle {

    id: root

    property string id: ""

    property alias title: builder.title

    signal closeRequested()

    color: ui.theme.backgroundPrimaryColor
    width: builder.contentItem ? builder.contentItem.implicitWidth : 450
    height: builder.contentItem ? builder.contentItem.implicitHeight : 200

    function apply() {
        if (!Boolean(builder.contentItem)) {
            return
        }

        builder.contentItem.apply()
    }

    Component.onCompleted: {
        builder.load(root.id, root)
    }

    EffectBuilder {
        id: builder

        onContentItemChanged: {
            if (!Boolean(contentItem)) {
                return
            }

            contentItem.closeRequested.connect(function() {
                root.closeRequested()
            })
        }

        onCloseRequested: root.closeRequested()
    }
}
