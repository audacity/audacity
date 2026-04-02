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

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "PluginManagerTopPanel"
        direction: NavigationPanel.Horizontal
        accessible.role: MUAccessible.ComboBox
        accessible.name: showModel.label + ", " + typeModel.label + ", " + categoryModel.label + ", " + searchField.accessible.name
    }

    signal searchTextChanged(string newText)

    function focusOnFirst() {
        dropdownsRepeater.itemAt(0).navigation.requestActive()
    }

    function readInfo() {
        accessibleInfo.ignored = false
        accessibleInfo.focused = true
    }

    AccessibleItem {
        id: accessibleInfo
        visualItem: root
        role: MUAccessible.ComboBox
        name: showModel.label + ", " + typeModel.label + ", " + categoryModel.label + ", " + searchField.accessible.name
    }

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
        id: dropdownsRepeater

        model: [showModel, typeModel, categoryModel]

        DropdownWithTitle {
            id: dropdown

            width: 150
            height: 30

            title: modelData.label
            model: modelData.options
            current: modelData.currentTitle
            allowOptionToggle: false

            navigation.name: modelData.label + "Dropdown"
            navigation.panel: root.navigationPanel

            Component.onCompleted: {
                // Don't know why `navigation.order: index` doesn't work here
                navigation.order = index
            }

            onHandleMenuItem: function (itemId) {
                modelData.select(itemId)
            }
        }
    }

    SearchField {
        id: searchField

        width: 200
        height: 30

        navigation.name: "SearchField"
        navigation.panel: root.navigationPanel
        navigation.order: dropdownsRepeater.count

        inputField.activeFocusOnPress: true

        onSearchTextChanged: {
            root.searchTextChanged(searchField.searchText)
        }
    }
}
