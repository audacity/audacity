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

    model: workspacesModel

    sourceComponentCallback: function(type) {
        switch(type) {
        case ToolBarItemType.ACTION: return controlComp
        }

        return null
    }

    Component {
        id: controlComp

        Loader {
            id: loader

            property var itemData: null

            sourceComponent: !root.isCompactMode ? dropdownComp : buttonComp

            Component {
                id: dropdownComp

                DropdownWithTitle {
                    property var itemData: loader.itemData

                    width: 228
                    height: root.rowHeight

                    title: qsTrc("projectscene", "Workspace")
                    model: Boolean(itemData) ? itemData.menuItems : null
                    current: Boolean(itemData) ? itemData.title : ""

                    allowOptionToggle: false

                    onHandleMenuItem: function(itemId) {
                        Qt.callLater(root.model.handleWorkspacesMenuItem, itemId)
                    }
                }
            }

            Component {
                id: buttonComp

                StyledToolBarItem {
                    itemData: loader.itemData

                    icon: IconCode.WORKSPACE
                    enabled: true
                }
            }
        }
    }
}
