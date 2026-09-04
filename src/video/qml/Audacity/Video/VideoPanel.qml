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

        StyledTextLabel {
            Layout.fillWidth: true
            visible: model.sourceMismatch
            text: model.warningText
            wrapMode: Text.WordWrap
            horizontalAlignment: Text.AlignLeft
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

            //! The frame on screen and how far it is from the playhead. The
            //! drift is the only place a sync error becomes a number the user
            //! can quote.
            StyledTextLabel {
                visible: model.hasVideo && surface.hasFrame
                text: surface.frameTimecode
                      + "  \u00b7  " + qsTrc("video", "frame %1").arg(surface.frameNumber)
                      + (Math.abs(surface.driftMs) >= 1
                         ? "  \u00b7  " + qsTrc("video", "%1 ms").arg(surface.driftMs > 0 ? "+" + surface.driftMs : surface.driftMs)
                         : "")
                opacity: 0.7
                elide: Text.ElideRight
            }

            StyledTextLabel {
                Layout.fillWidth: true
                text: model.hasVideo && !surface.hasFrame ? model.statusText : ""
                opacity: 0.7
                elide: Text.ElideRight
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

            // Stays alongside "Attach video…" rather than replacing it, so
            // that installing FFmpeg and trying again is one panel visit.
            FlatButton {
                text: qsTrc("video", "Get FFmpeg…")
                visible: model.needsFFmpeg
                navigation.panel: navPanel
                navigation.order: 3
                onClicked: model.openFFmpegPreferences()
            }
        }
    }
}
