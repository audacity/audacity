import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene


StyledPopupView {
    id: root
    objectName: "AddNewTrackPanel"

    contentWidth: trackTypeOpts.width
    contentHeight: trackTypeOpts.height

    modal: true

    property alias popupAnchorItem: root.anchorItem

    signal createTrack(type : int)

    NavigationPanel {
        id: navPanel
        name: "AddNewTrackPopup"
        enabled: root.isOpened
        direction: NavigationPanel.Horizontal
        section: root.navigationSection
    }

    onOpened: {
        navPanel.requestActive()
    }

    RowLayout {
        id: trackTypeOpts

        spacing: 10

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
                Layout.margins: 2

                navigation.name: "TrackType" + index
                navigation.panel: navPanel
                navigation.column: index

                accentButton: false
                enabled: modelData.enabled
                icon: modelData.icon
                text: modelData.text
                onClicked: {
                    createTrack(modelData.type)
                }

                navigation.onNavigationEvent: function(event) {
                    if (event.type === NavigationEvent.Trigger) {
                        createTrack(modelData.type)
                        event.accepted = true
                        root.close()
                    }
                }
            }
        }
    }
}
