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

import Muse.Ui 1.0
import Muse.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Style used for import")

    navigation.direction: NavigationPanel.Both

    property string styleFileImportPath: ""
    property string fileChooseTitle: ""
    property var filePathFilter: null
    property string fileDirectory: ""

    signal styleFileImportPathChangeRequested(string path)

    QtObject {
        id: prv

        property bool useStyleFile: root.styleFileImportPath !== ""
    }

    RoundedRadioButton {
        id: builtInStyleButton
        width: root.columnWidth

        text: qsTrc("appshell/preferences", "Built-in style")
        checked: !prv.useStyleFile

        navigation.name: "BuiltInStyleButton"
        navigation.panel: root.navigation
        navigation.row: 0
        navigation.column: 0

        onToggled: {
            prv.useStyleFile = false
            root.styleFileImportPathChangeRequested("")
        }
    }

    Row {
        width: parent.width
        spacing: root.columnSpacing

        RoundedRadioButton {
            id: useStyleFileButton

            width: root.columnWidth
            anchors.verticalCenter: parent.verticalCenter

            text: qsTrc("appshell/preferences", "Use style file:")
            checked: prv.useStyleFile

            navigation.name: "UseStyleButton"
            navigation.panel: root.navigation
            navigation.row: 1
            navigation.column: 0

            onToggled: {
                prv.useStyleFile = true
            }
        }

        FilePicker {
            id: styleFilePicker

            pathFieldWidth: root.columnWidth
            anchors.verticalCenter: parent.verticalCenter

            dialogTitle: root.fileChooseTitle
            filter: root.filePathFilter
            dir: root.fileDirectory

            path: root.styleFileImportPath

            enabled: prv.useStyleFile

            navigation: root.navigation
            navigationRowOrderStart: 1
            navigationColumnOrderStart: 1

            onPathEdited: function(newPath) {
                root.styleFileImportPathChangeRequested(newPath)
            }
        }
    }
}
