import QtQuick

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
            width: clipWidthData
            x: clipLeftData

            title: clipTitleData

            context : root.context
            trackId : root.trackId
            clipIndex: clipIndexData
        }
    }
}
