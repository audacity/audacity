import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene


StyledPopupView {
    id: root
    objectName: "AddNewTrackPanel"

    contentWidth: content.width
    contentHeight: content.height

    modal: true

    property alias popupAnchorItem: root.anchorItem

    signal createTrack(type : int)

    ColumnLayout {
        id: content

        spacing: 12

        RowLayout {

            width: parent.width
            height: 72
            spacing: 12

            Repeater {
                model: [
                    { type: TrackType.MONO, icon: IconCode.MICROPHONE, text: qsTrc("projectscene", "Mono"), enabled: true },
                    { type: TrackType.STEREO, icon: IconCode.MICROPHONE, text: qsTrc("projectscene", "Stereo"), enabled: true },
                    { type: TrackType.LABEL, icon: IconCode.LOOP_IN, text: qsTrc("projectscene", "Label"), enabled: false }
                ]

                FlatButton {
                    Layout.preferredWidth: 80
                    Layout.preferredHeight: 72
                    Layout.fillHeight: true

                    accentButton: false
                    enabled: modelData.enabled
                    icon: modelData.icon
                    text: modelData.text
                    onClicked: {
                        createTrack(modelData.type)
                    }
                }
            }
        }
    }
}
