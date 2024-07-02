/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

StyledToolBarView {
    navigationPanel.name: "ProjectToolBar"
    navigationPanel.accessible.name: qsTrc("projectscene", "Project toolbar")

    spacing: 2
    rowHeight: 36

    model: ProjectToolBarModel { }
}
