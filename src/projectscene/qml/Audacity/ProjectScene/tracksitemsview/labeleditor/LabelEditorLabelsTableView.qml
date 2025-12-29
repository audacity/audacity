/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.UiComponents
import Audacity.ProjectScene

StyledTableView {
    id: root

    property bool hasSelection: tableViewModel.selectionModel.hasSelection

    headerCapitalization: Font.MixedCase

    function addNewLabel() {
        tableViewModel.addNewLabel()
    }

    function removeSelectedLabels() {
        tableViewModel.removeSelectedLabels()
    }

    function exportLabels() {
        tableViewModel.exportLabels()
    }

    function importLabels() {
        tableViewModel.importLabels()
    }

    LabelsTableViewModel {
        id: tableViewModel
    }

    Component.onCompleted: {
        tableViewModel.load()
    }

    model: tableViewModel

    sourceComponentCallback: function(type) {
        switch(type) {
        case LabelsTableViewCellType.Track: return trackComp
        case LabelsTableViewCellType.Timecode: return timecodeComp
        case LabelsTableViewCellType.Frequency: return frequencyComp
        }

        return null
    }

    Component {
        id: trackComp

        DropdownWithTitle {
            id: item

            property var itemData
            property var val
            property int row
            property int column

            property NavigationPanel navigationPanel
            property int navigationRow
            property int navigationColumnStart

            property string accessibleName: current

            signal changed(string stub)
            signal editingFinished()

            model: Boolean(itemData) ? itemData.availableTracks : null
            current: val

            allowOptionToggle: false

            navigation.panel: navigationPanel
            navigation.order: navigationRow
            navigation.column: navigationColumnStart

            onHandleMenuItem: function(itemId) {
                Qt.callLater(tableViewModel.handleTrackMenuItem, row, column, itemId)
            }

            onIsOpenedChanged: {
                if (!isOpened) {
                    Qt.callLater(editingFinished)
                }
            }
        }
    }

    Component {
        id: timecodeComp

        Timecode {
            id: item

            property var itemData
            property double val
            property int row
            property int column

            property NavigationPanel navigationPanel
            property int navigationRow
            property int navigationColumnStart

            property string accessibleName: val

            signal changed(double value)
            signal editingFinished()

            height: 24

            value: val

            currentFormat: parseInt(tableViewModel.headerData(column, Qt.Horizontal).currentFormatId)
            showMenu: false

            sampleRate: Boolean(itemData) ? itemData.sampleRate : 0
            tempo: Boolean(itemData) ? itemData.tempo : 0
            upperTimeSignature: Boolean(itemData) ? itemData.upperTimeSignature : -1
            lowerTimeSignature: Boolean(itemData) ? itemData.lowerTimeSignature : -1

            navigation.panel: navigationPanel
            navigation.enabled: root.currentEditedCell === item
            navigation.row: navigationRow
            navigation.column: navigationColumnStart

            navigation.onActiveChanged: function(active) {
                if (active) {
                    navigation.triggered()
                }
            }

            onValueChangeRequested: function(newValue) {
                item.changed(newValue)
            }

            onValueEditingFinished: {
                Qt.callLater(editingFinished)
            }
        }
    }

    Component {
        id: frequencyComp

        Frequency {
            id: item

            property var itemData
            property double val
            property int row
            property int column

            property NavigationPanel navigationPanel
            property int navigationRow
            property int navigationColumnStart

            property string accessibleName: val

            signal changed(double value)
            signal editingFinished()

            height: 24

            value: val

            currentFormat: parseInt(tableViewModel.headerData(column, Qt.Horizontal).currentFormatId)
            showMenu: false

            sampleRate: Boolean(itemData) ? itemData.sampleRate : 0
            tempo: Boolean(itemData) ? itemData.tempo : 0
            upperTimeSignature: Boolean(itemData) ? itemData.upperTimeSignature : -1
            lowerTimeSignature: Boolean(itemData) ? itemData.lowerTimeSignature : -1

            navigation.panel: navigationPanel
            navigation.enabled: root.currentEditedCell === item
            navigation.row: navigationRow
            navigation.column: navigationColumnStart

            navigation.onActiveChanged: function(active) {
                if (active) {
                    navigation.triggered()
                }
            }

            onValueChangeRequested: function(newValue) {
                item.changed(newValue)
            }

            onValueEditingFinished: {
                Qt.callLater(editingFinished)
            }
        }
    }
}
