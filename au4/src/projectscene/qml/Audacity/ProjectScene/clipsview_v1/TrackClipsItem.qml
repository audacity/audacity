import QtQuick

import Audacity.ProjectScene

Item {

    id: root

    property int trackId: -1
    property var context: null

    WaveTrackView {

        anchors.fill: parent
        context : root.context
        trackId : root.trackId
    }

}
