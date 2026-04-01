/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.UiComponents

Row {
    id: root

    padding: 8
    spacing: 16

    signal searchTextChanged(string newText)

    PluginManagerTopPanelModel {
        id: topPanelModel
    }

    Repeater {
        model: [topPanelModel.showModel, topPanelModel.typeModel, topPanelModel.categoryModel]

        DropdownWithTitle {
            width: 150
            height: 30

            title: modelData.label
            model: modelData.options
            current: modelData.currentTitle
            allowOptionToggle: false

            onHandleMenuItem: function (itemId) {
                modelData.select(itemId)
            }
        }
    }

    SearchField {
        id: searchField

        width: 200
        height: 30

        inputField.activeFocusOnPress: true

        onSearchTextChanged: {
            root.searchTextChanged(searchField.searchText)
        }
    }
}
