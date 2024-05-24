/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Item {
    id: root

    signal activeFocusRequested()

    width: view.width
    height: view.height

    Component.onCompleted: {
        toolbarModel.load()
    }

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "ProjectToolBar"
        enabled: root.enabled && root.visible
        accessible.name: qsTrc("projectscene", "Project toolbar")
        onActiveChanged: function(active) {
            if (active) {
                root.activeFocusRequested()
                root.forceActiveFocus()
            }
        }
    }

    ProjectToolBarModel {
        id: toolbarModel
    }

    ListView {
        id: view

        width: contentWidth
        height: contentItem.childrenRect.height

        orientation: Qt.Horizontal
        interactive: false
        spacing: 2

        model: toolbarModel

        delegate: FlatButton {
            height: 30

            property var item: Boolean(model) ? model.itemRole : null

            text: Boolean(item) ? item.title : ""
            icon: Boolean(item) ? item.icon : IconCode.NONE
            iconFont: ui.theme.toolbarIconsFont

            toolTipTitle: Boolean(item) ? item.title : ""
            toolTipDescription: Boolean(item) ? item.description : ""
            toolTipShortcut: Boolean(item) ? item.shortcuts : ""

            enabled: Boolean(item) ? item.enabled : false

            textFont: ui.theme.largeBodyFont

            navigation.panel: root.navigationPanel
            navigation.name: toolTipTitle
            navigation.order: model.index
            accessible.name: (item.checkable ? (item.checked ? item.title + "  " + qsTrc("global", "On") :
                                                               item.title + "  " + qsTrc("global", "Off")) : item.title)

            transparent: true
            orientation: Qt.Horizontal

            onClicked: {
                toolbarModel.handleMenuItem(item.id)
            }
        }
    }
}
