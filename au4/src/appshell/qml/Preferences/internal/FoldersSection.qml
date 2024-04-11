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

import Muse.Ui 1.0
import Muse.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Folders")

    navigation.direction: NavigationPanel.Both

    property alias model: view.model

    StyledListView {
        id: view

        width: parent.width
        height: contentHeight

        spacing: 4
        interactive: false

        delegate: RowLayout {
            width: ListView.view.width
            height: 30

            spacing: root.columnSpacing

            StyledTextLabel {
                id: titleLabel
                Layout.preferredWidth: root.columnWidth
                text: model.title + ":"
                horizontalAlignment: Text.AlignLeft
            }

            FilePicker {
                Layout.fillWidth: true

                pickerType: model.isMultiDirectories ? FilePicker.PickerType.MultipleDirectories : FilePicker.PickerType.Directory
                dialogTitle: qsTrc("appshell/preferences", "Choose %1 folder").arg(model.title)
                dir: model.dir

                path: model.path

                navigation: root.navigation
                navigationRowOrderStart: model.index
                pathFieldTitle: titleLabel.text

                onPathEdited: function(newPath) {
                    model.path = newPath
                }
            }
        }
    }
}
