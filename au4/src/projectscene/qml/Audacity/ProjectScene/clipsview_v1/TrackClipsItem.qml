import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Item {

    id: root

    property alias trackId: clipsModel.trackId
    property alias context: clipsModel.context

    ClipsListModel {
        id: clipsModel
    }

    Component.onCompleted: {
        clipsModel.load()
    }

    Repeater {
        model: clipsModel

        delegate: ClipItem {

            height: parent.height
            width: model.clipWidthData
            x: model.clipLeftData

            title: model.clipTitleData

            context: root.context
            clipKey: model.clipKeyData

            onPositionChanged: function(x) {
                console.log("[ClipItem] onXChanged: " + x)
                model.clipLeftData = x
            }
        }
    }

    SeparatorLine { anchors.bottom: parent.bottom }
}
