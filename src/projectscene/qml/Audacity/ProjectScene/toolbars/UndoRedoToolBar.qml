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
    // The buttons follow the theme's (scaled) button size; a fixed row clipped them.
    rowHeight: Math.max(28, ui.theme.defaultButtonSize)

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
