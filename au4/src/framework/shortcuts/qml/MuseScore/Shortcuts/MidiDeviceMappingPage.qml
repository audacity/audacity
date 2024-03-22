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
import QtQuick.Layouts 1.15

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0
import MuseScore.Shortcuts 1.0

import "internal"

Item {
    id: root

    property NavigationSection navigationSection: null
    property int navigationOrderStart: 0

    function apply() {
        return mappingsModel.apply()
    }

    function reset() {
        mappingsModel.reset()
    }

    EditMidiMappingDialog {
        id: editMappingDialog

        function startEditCurrentAction() {
            editMappingDialog.startEdit(mappingsModel.currentAction())
        }

        onMapToEventRequested: function(event) {
            mappingsModel.mapCurrentActionToMidiEvent(event)
        }
    }

    MidiDeviceMappingModel {
        id: mappingsModel

        selection: view.selection
    }

    Component.onCompleted: {
        mappingsModel.load()
    }

    ColumnLayout {
        anchors.fill: parent

        spacing: 20

        MidiMappingTopPanel {
            useRemoteControl: mappingsModel.useRemoteControl

            navigation.section: root.navigationSection
            navigation.order: root.navigationOrderStart + 1

            onUseRemoteControlChangeRequested: function(checked) {
                mappingsModel.useRemoteControl = checked
            }
        }

        ValueList {
            id: view

            Layout.fillWidth: true
            Layout.fillHeight: true

            enabled: mappingsModel.useRemoteControl
            readOnly: true

            keyRoleName: "title"
            keyTitle: qsTrc("shortcuts", "action")
            valueRoleName: "status"
            valueTitle: qsTrc("shortcuts", "status")
            iconRoleName: "icon"
            valueEnabledRoleName: "enabled"

            model: mappingsModel

            navigationSection: root.navigationSection
            navigationOrderStart: root.navigationOrderStart + 2

            onHandleItem: {
                editMappingDialog.startEditCurrentAction()
            }
        }

        MidiMappingBottomPanel {
            Layout.alignment: Qt.AlignRight

            enabled: mappingsModel.useRemoteControl

            canEditAction: mappingsModel.canEditAction

            navigation.section: root.navigationSection
            //! NOTE: 4 because ShortcutsList have two panels(header and content)
            navigation.order: root.navigationOrderStart + 4

            onEditActionRequested: {
                editMappingDialog.startEditCurrentAction()
            }

            onClearSelectedActionsRequested: {
                mappingsModel.clearSelectedActions()
            }

            onClearAllActionsRequested: {
                mappingsModel.clearAllActions()
            }
        }
    }
}
