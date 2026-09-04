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

            //! The frame on screen and how far it is from the playhead. The
            //! drift is the only place a sync error becomes a number the user
            //! can quote, so it leads the row rather than trailing it.
            //!
            //! Monospaced and with each field at a fixed width, because these
            //! digits change many times a second and a proportional font makes
            //! the whole row twitch as they do.
            RowLayout {
                spacing: 8
                visible: model.hasVideo && surface.hasFrame

                //! Every field here is a number that changes many times a
                //! second. In a proportional face each digit is a different
                //! width, so the text reflows on every frame and the whole row
                //! twitches. A fixed-pitch face is the fix; the style hint is
                //! what makes it resolve to one on every platform rather than
                //! only where a family literally named "Monospace" exists.
                StyledTextLabel {
                    id: timecodeLabel

                    text: surface.frameTimecode
                    font.family: "Monospace"
                    font.styleHint: Font.TypeWriter
                    font.pixelSize: ui.theme.bodyFont.pixelSize + 2
                    font.bold: true
                    horizontalAlignment: Text.AlignLeft
                }

                StyledTextLabel {
                    text: qsTrc("video", "frame %1").arg(surface.frameNumber)
                    font.family: "Monospace"
                    font.styleHint: Font.TypeWriter
                    opacity: 0.6
                    horizontalAlignment: Text.AlignLeft
                }

                StyledTextLabel {
                    visible: Math.abs(surface.driftMs) >= 1
                    text: qsTrc("video", "%1 ms").arg(surface.driftMs > 0 ? "+" + surface.driftMs : surface.driftMs)
                    font.family: "Monospace"
                    font.styleHint: Font.TypeWriter
                    opacity: 0.6
                    horizontalAlignment: Text.AlignLeft
                }

                //! Only shown when it is not zero, so the common case stays
                //! uncluttered and a shifted one is impossible to miss.
                StyledTextLabel {
                    visible: model.offsetText.length > 0
                    text: qsTrc("video", "offset %1").arg(model.offsetText)
                    font.family: "Monospace"
                    font.styleHint: Font.TypeWriter
                    opacity: 0.6
                    horizontalAlignment: Text.AlignLeft
                }
            }

            StyledTextLabel {
                text: model.hasVideo && !surface.hasFrame ? model.statusText : ""
                opacity: 0.7
                elide: Text.ElideRight
                Layout.fillWidth: true
                horizontalAlignment: Text.AlignLeft
            }

            Item { Layout.fillWidth: true }

            //! Which file is on screen matters far less than where in it we
            //! are, so it sits at the far end and stays quiet.
            StyledTextLabel {
                Layout.maximumWidth: root.width * 0.4
                horizontalAlignment: Text.AlignRight
                text: model.hasVideo ? model.sourceName : ""
                opacity: 0.45
                elide: Text.ElideMiddle
            }

            FlatButton {
                text: qsTrc("video", "Attach video…")
                visible: !model.hasVideo
                navigation.panel: navPanel
                navigation.order: 1
                onClicked: model.attachVideo()
            }

            // Stays alongside "Attach video…" rather than replacing it, so
            // that installing FFmpeg and trying again is one panel visit.
            FlatButton {
                text: qsTrc("video", "Get FFmpeg…")
                visible: model.needsFFmpeg
                navigation.panel: navPanel
                navigation.order: 2
                onClicked: model.openFFmpegPreferences()
            }
        }
    }
}
