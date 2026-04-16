/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import Muse.UiComponents
import Audacity.Effects

StyledTableView {
    id: root

    required property PluginManagerTableViewModel tableViewModel

    headerCapitalization: Font.MixedCase

    model: tableViewModel
    horizontalHeaderNavigationEnabled: true

    sourceComponentCallback: function (type) {
        switch (type) {
        case PluginManagerTableViewCellType.Enabled:
            return enabledComp
        }
        return null
    }

    onHorizontalHeaderClicked: function (column) {
        tableViewModel.toggleColumnSort(column)
    }

    Component {
        id: enabledComp

        CheckBox {
            id: item

            property var itemData
            property var val
            property int row
            property int column

            property NavigationPanel navigationPanel
            property int navigationRow
            property int navigationColumnStart

            property string accessibleName: val ? qsTr("Enabled") : qsTr("Disabled")

            signal changed(string stub)
            signal editingFinished

            navigation.panel: navigationPanel
            navigation.enabled: root.currentEditedCell === item
            navigation.order: navigationRow
            navigation.column: navigationColumnStart

            checked: val
            onClicked: {
                tableViewModel.handleEdit(row, column)
            }
        }
    }
}
