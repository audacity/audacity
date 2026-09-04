/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.Video

//! The video as an item inside the playback toolbar, alongside the transport
//! buttons, the timecode and the snap controls. Turned on and off from
//! "Customize toolbar" like any other item there.
//!
//! A picture and nothing else: the dockable panel is where the controls, the
//! readouts and the "..." menu live. This is for keeping half an eye on the
//! video while working on the audio.
Item {
    id: root

    //! Set by StyledToolBarView on load. Unused here - the picture comes from
    //! the video service, not from the toolbar item - but the view assigns it
    //! unconditionally, so it has to exist.
    property var itemData: null

    //! The toolbar row gives the height; the width follows from what is being
    //! shown, so the picture is never stretched and never leaves a gap.
    implicitHeight: model.toolbarHeight
    implicitWidth: surface.hasFrame && surface.frameAspect > 0
                   ? Math.max(32, Math.round(root.height * surface.frameAspect))
                   : Math.round(root.height * 16 / 9)

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

    //! Right click chooses how large the thumbnail is. The cap is what fits
    //! in the toolbar's row; the dockable panel is there for a real view.
    MouseArea {
        anchors.fill: parent
        acceptedButtons: Qt.RightButton
        onClicked: function(mouse) {
            sizeMenu.items = [
                { "id": "20", "title": qsTrc("video", "Small"), "checkable": true, "checked": model.toolbarHeight === 20 },
                { "id": "28", "title": qsTrc("video", "Medium"), "checkable": true, "checked": model.toolbarHeight === 28 },
                { "id": "36", "title": qsTrc("video", "Large"), "checkable": true, "checked": model.toolbarHeight === 36 },
                { "id": "44", "title": qsTrc("video", "Extra large"), "checkable": true, "checked": model.toolbarHeight === 44 }
            ]
            sizeMenu.show(Qt.point(mouse.x, mouse.y))
        }
    }

    ContextMenuLoader {
        id: sizeMenu

        onHandleMenuItem: function(itemId) {
            model.toolbarHeight = parseInt(itemId, 10)
        }
    }

    //! Nothing attached is an ordinary state here, so it says so quietly
    //! rather than showing an unexplained black rectangle.
    Rectangle {
        anchors.fill: parent
        visible: !surface.hasFrame
        color: "transparent"
        border.width: 1
        border.color: ui.theme.strokeColor
        radius: 2

        StyledTextLabel {
            anchors.centerIn: parent
            width: parent.width - 4
            text: qsTrc("video", "No video")
            font.pixelSize: ui.theme.bodyFont.pixelSize - 2
            opacity: 0.5
            elide: Text.ElideRight
            horizontalAlignment: Text.AlignHCenter
        }
    }
}
