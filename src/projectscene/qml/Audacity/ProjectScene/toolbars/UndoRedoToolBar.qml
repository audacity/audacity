/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

StyledToolBarView {
    id: root

    navigationPanel.name: "UndoRedoToolBar"
    navigationPanel.accessible.name: qsTrc("projectscene", "Undo and redo")

    spacing: 0
    rowHeight: 28

    model: UndoRedoToolBarModel {}

    sourceComponentCallback: function (type) {
        switch (type) {
        case ToolBarItemType.ACTION:
            return controlComp
        }

        return null
    }

    Component {
        id: controlComp

        StyledToolBarItem {
            width: ui.theme.defaultButtonSize
            height: width

            navigation.panel: root.navigationPanel
        }
    }
}
