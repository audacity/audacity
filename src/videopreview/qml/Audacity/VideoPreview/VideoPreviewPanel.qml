/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.VideoPreview

Item {
    id: root

    property alias navigationSection: navPanel.section
    property alias navigationOrderStart: navPanel.order

    NavigationPanel {
        id: navPanel
        name: "VideoPreviewPanel"
        direction: NavigationPanel.Vertical
        enabled: root.enabled && root.visible
    }

    VideoPreviewModel {
        id: previewModel
    }

    Component.onCompleted: {
        previewModel.init()
        previewFrame.init()
    }

    Rectangle {
        anchors.fill: parent
        color: ui.theme.backgroundPrimaryColor
    }

    VideoPreviewItem {
        id: previewFrame
        anchors.fill: parent
        visible: previewModel.hasFrame
    }

    StyledTextLabel {
        anchors.centerIn: parent
        width: Math.min(parent.width - 24, 260)
        visible: !previewModel.hasFrame

        text: previewModel.stateText
        horizontalAlignment: Text.AlignHCenter
        wrapMode: Text.WordWrap
        color: ui.theme.fontPrimaryColor
        opacity: 0.72
    }
}
