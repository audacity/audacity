/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

import "internal"

StyledToolBarView {
    id: root

    property bool isCompactMode: false

    navigationPanel.name: "WorkspacesToolBar"
    navigationPanel.accessible.name: qsTrc("projectscene", "Workspaces toolbar")

    spacing: 0
    rowHeight: 28

    WorkspacesToolBarModel {
        id: workspacesModel
    }

    model: isCompactMode ? null : workspacesModel

    sourceComponentCallback: function(type) {
        switch(type) {
        case ToolBarItemType.ACTION: return controlComp
        }

        return null
    }

    Component {
        id: controlComp

        DropdownWithTitle {
            id: control

            property var itemData: null

            width: 228
            height: root.rowHeight

            title: qsTrc("projectscene", "Workspace")
            model: Boolean(control.itemData) ? control.itemData.menuItems : null
            current: Boolean(control.itemData) ? control.itemData.title : ""

            allowOptionToggle: false

            onHandleMenuItem: function(itemId) {
                Qt.callLater(root.model.handleWorkspacesMenuItem, itemId)
            }
        }
    }
}

