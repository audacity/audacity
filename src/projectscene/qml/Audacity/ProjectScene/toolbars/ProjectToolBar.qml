/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

StyledToolBarView {
    id: root

    property alias isCompactMode: toolBarModel.isCompactMode

    navigationPanel.name: "ProjectToolBar"
    navigationPanel.accessible.name: qsTrc("projectscene", "Project toolbar")

    spacing: 2
    rowHeight: 28

    model: ProjectToolBarModel {
        id: toolBarModel

        readonly property int bottomMargin: 10

        onOpenAudioSetupContextMenu: {
            audioSetupContextMenuLoader.show(Qt.point(root.width / 3, root.rowHeight + bottomMargin), audioSetupContextMenuModel.items)
        }
    }

    AudioSetupContextMenuModel {
        id: audioSetupContextMenuModel
    }

    ContextMenuLoader {
        id: audioSetupContextMenuLoader

        onHandleMenuItem: function(itemId) {
            audioSetupContextMenuModel.handleMenuItem(itemId)
        }
    }

    Component.onCompleted: {
        audioSetupContextMenuModel.load()
    }
}
