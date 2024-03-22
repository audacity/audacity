/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0
import MuseScore.Shortcuts 1.0

import "internal"

Item {
    id: root

    property NavigationSection navigationSection: null
    property int navigationOrderStart: 0

    property string shortcutCodeKey: ""

    ShortcutsModel {
        id: shortcutsModel

        selection: shortcutsView.sourceSelection
    }

    function apply() {
        return shortcutsModel.apply()
    }

    function reset() {
        shortcutsModel.reset()
    }

    Component.onCompleted: {
        shortcutsModel.load()

        topPanel.setSearchText(root.shortcutCodeKey)
    }

    QtObject {
        id: prv

        readonly property int buttonMinWidth: 104
    }

    EditShortcutDialog {
        id: editShortcutDialog

        onApplySequenceRequested: function(newSequence, conflictShortcutIndex) {
            shortcutsModel.applySequenceToCurrentShortcut(newSequence, conflictShortcutIndex)
        }

        property bool canEditCurrentShortcut: Boolean(shortcutsModel.currentShortcut)

        function startEditCurrentShortcut() {
            editShortcutDialog.startEdit(shortcutsModel.currentShortcut, shortcutsModel.shortcuts())
        }
    }

    ColumnLayout {
        anchors.fill: parent

        spacing: 20

        ShortcutsTopPanel {
            id: topPanel

            Layout.fillWidth: true
            Layout.preferredHeight: childrenRect.height

            canEditCurrentShortcut: editShortcutDialog.canEditCurrentShortcut
            canClearCurrentShortcuts: shortcutsView.hasSelection

            buttonMinWidth: prv.buttonMinWidth

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 1

            onStartEditCurrentShortcutRequested: {
                editShortcutDialog.startEditCurrentShortcut()
            }

            onClearSelectedShortcutsRequested: {
                shortcutsModel.clearSelectedShortcuts()
            }
        }

        ShortcutsList {
            id: shortcutsView

            Layout.fillWidth: true
            Layout.fillHeight: true

            sourceModel: shortcutsModel
            searchText: topPanel.searchText

            navigationSection: root.navigationSection
            navigationOrderStart: root.navigationOrderStart + 2

            onStartEditCurrentShortcutRequested: {
                editShortcutDialog.startEditCurrentShortcut()
            }
        }

        ShortcutsBottomPanel {
            Layout.fillWidth: true
            Layout.preferredHeight: childrenRect.height

            canResetCurrentShortcut: shortcutsView.hasSelection

            buttonMinWidth: prv.buttonMinWidth

            navigation.section: root.navigationSection
            //! NOTE: 4 because ShortcutsList have two panels(header and content)
            navigation.order: root.navigationOrderStart + 4

            onImportShortcutsFromFileRequested: {
                shortcutsModel.importShortcutsFromFile()
            }

            onExportShortcutsToFileRequested: {
                shortcutsModel.exportShortcutsToFile()
            }

            onResetToDefaultSelectedShortcuts: {
                shortcutsModel.resetToDefaultSelectedShortcuts()
            }
        }
    }
}
