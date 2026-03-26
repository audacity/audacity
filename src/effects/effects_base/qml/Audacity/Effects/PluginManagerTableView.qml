/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import Muse.UiComponents
import Audacity.Effects

StyledTableView {
    id: root

    headerCapitalization: Font.MixedCase

    PluginManagerTableViewModel {
        id: tableViewModel
    }

    model: tableViewModel

    sourceComponentCallback: function (type) {
        switch (type) {
        case PluginManagerTableViewCellType.Enabled:
            return enabledComp
        }
        return null
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

            checked: val
            onClicked: {
                tableViewModel.handleEdit(row, column)
            }
        }
    }
}
