/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

StyledToolBarView {
    property alias isCompactMode: toolBarModel.isCompactMode

    navigationPanel.name: "ProjectToolBar"
    navigationPanel.accessible.name: qsTrc("projectscene", "Project toolbar")

    spacing: 2
    rowHeight: 28

    model: ProjectToolBarModel {
        id: toolBarModel
    }
}
