/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Video

Item {
    id: root

    property alias navigationSection: navPanel.section
    property alias navigationOrderStart: navPanel.order

    NavigationPanel {
        id: navPanel
        name: "VideoPanel"
        direction: NavigationPanel.Vertical
        enabled: root.enabled && root.visible
    }

    VideoPanelModel {
        id: model
    }

    Component.onCompleted: {
        model.init()
    }

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 8
        spacing: 8

        VideoSurfaceItem {
            id: surface

            Layout.fillWidth: true
            Layout.fillHeight: true

            StyledTextLabel {
                anchors.centerIn: parent
                width: parent.width - 24
                visible: !surface.hasFrame
                text: surface.outOfRange ? qsTrc("video", "No video at this position") : model.statusText
                horizontalAlignment: Text.AlignHCenter
                wrapMode: Text.WordWrap
            }
        }

        RowLayout {
            Layout.fillWidth: true
            spacing: 8

            StyledTextLabel {
                Layout.fillWidth: true
                horizontalAlignment: Text.AlignLeft
                text: model.hasVideo ? model.sourceName : ""
                elide: Text.ElideMiddle
            }

            StyledTextLabel {
                text: model.hasVideo ? model.statusText : ""
                opacity: 0.7
            }

            FlatButton {
                text: qsTrc("video", "Attach video…")
                visible: !model.hasVideo
                navigation.panel: navPanel
                navigation.order: 1
                onClicked: model.attachVideo()
            }

            FlatButton {
                text: qsTrc("video", "Detach")
                visible: model.hasVideo
                navigation.panel: navPanel
                navigation.order: 2
                onClicked: model.detachVideo()
            }
        }
    }
}
