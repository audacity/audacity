import QtQuick

import Audacity.ProjectScene

Item {

    Component.onCompleted: {
        clipsModel.load()
    }

    ClipsModel {
        id: clipsModel
    }

    Column {
        anchors.fill: parent
        Repeater {
            model: clipsModel.tracks
            delegate: Item {

                property var track: modelData

                height: 80
                width: 650
                //color: "#800000"

                Row {
                    anchors.fill: parent
                    spacing: 16

                    Repeater {
                        model: track.clips
                        delegate: Rectangle {
                            id: clipItem
                            property var clip: modelData

                            height: 80
                            width: 160
                            border.width: 2
                            border.color: "#666"
                            color: "#363B4D"

                            WaveView {
                                anchors.fill: parent
                                source: clipItem.clip.waveSource
                            }
                        }
                    }
                }
            }
        }
    }
}
