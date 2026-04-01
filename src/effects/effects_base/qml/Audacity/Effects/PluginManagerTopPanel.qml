/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Layouts
import Muse.UiComponents
import Audacity.UiComponents
import Audacity.Effects

Row {
    id: root

    padding: 8
    spacing: 16

    required property PluginManagerTableViewModel tableViewModel

    signal searchTextChanged(string newText)

    Component.onCompleted: {
        tableViewModel.enabledDisabledSelectedIndex = Qt.binding(function () {
            return showModel.selectedIndex
        })
        tableViewModel.effectFamilySelectedIndex = Qt.binding(function () {
            return typeModel.selectedIndex
        })
        tableViewModel.effectTypeSelectedIndex = Qt.binding(function () {
            return categoryModel.selectedIndex
        })
    }

    DropdownOptionsModel {
        id: showModel
        label: qsTrc("effects", "Show")
        options: tableViewModel.enabledDisabledOptions
    }

    DropdownOptionsModel {
        id: typeModel
        label: qsTrc("effects", "Type")
        options: tableViewModel.effectFamilyOptions
    }

    DropdownOptionsModel {
        id: categoryModel
        label: qsTrc("effects", "Category")
        options: tableViewModel.effectTypeOptions
    }

    Repeater {
        model: [showModel, typeModel, categoryModel]

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
