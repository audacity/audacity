/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
 *
 * Copyright (C) 2025 Audacity BVBA and others
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

    navigation.direction: NavigationPanel.Horizontal

    property string temporaryPath: ""

    onTemporaryPathChanged: {
        dirPicker.path = temporaryPath
    }

    signal temporaryFilesLocationChanged(string newPath)

    Column {
        spacing: 12

        StyledTextLabel {
            text: qsTrc("appshell/preferences", "Temporary files location")
        }

        Row {
            FilePicker {
                id: dirPicker

                pickerType: FilePicker.PickerType.Directory
                pathFieldWidth: root.columnWidth
                spacing: root.columnSpacing

                dialogTitle: qsTrc("appshell/preferences", "Choose temporary files location")

                path: temporaryPath
                dir: temporaryPath

                navigation: root.navigation
                navigationColumnOrderStart: 1

                onPathEdited: function(newPath) {
                    path = newPath
                    root.temporaryFilesLocationChanged(newPath)
                }
            }
        }

        StyledTextLabel {
            text: qsTrc("appshell/preferences", "Folder in which unsaved projects and other data are kept")
            color: ui.theme.fontSecondaryColor
        }
    }
}
