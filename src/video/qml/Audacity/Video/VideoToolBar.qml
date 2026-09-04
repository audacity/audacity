/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.Video

//! A compact always-visible picture that lives in the toolbar row, for
//! keeping an eye on the video while working on the audio. The dockable
//! panel remains the place to actually watch it: this one is sized by the
//! height of the toolbar strip, so it is small by design.
Item {
    id: root

    property alias navigationPanel: navPanel
    property bool isCompactMode: false

    //! The toolbar row gives the height; the width follows from what is being
    //! shown, so the picture is never stretched and never leaves a gap.
    implicitHeight: 40
    implicitWidth: surface.hasFrame && surface.frameAspect > 0
                   ? Math.max(48, Math.round(root.height * surface.frameAspect))
                   : Math.round(root.height * 16 / 9)

    NavigationPanel {
        id: navPanel
        name: "VideoToolBar"
        enabled: root.enabled && root.visible
    }

    VideoPanelModel {
        id: model
    }

    Component.onCompleted: {
        model.init()
    }

    VideoSurfaceItem {
        id: surface
        anchors.fill: parent
    }

    //! Nothing attached is the ordinary state for this toolbar, so it says so
    //! quietly rather than showing a black rectangle with no explanation.
    StyledTextLabel {
        anchors.centerIn: parent
        width: parent.width - 8
        visible: !surface.hasFrame
        text: qsTrc("video", "No video")
        opacity: 0.5
        elide: Text.ElideRight
        horizontalAlignment: Text.AlignHCenter
    }
}
