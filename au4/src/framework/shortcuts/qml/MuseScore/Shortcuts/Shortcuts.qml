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

import MuseScore.Shortcuts 1.0

QtObject {
    id: root

    Component.onCompleted: {
        shortcutsModel.init()
    }

    property var objects: []

    property ShortcutsInstanceModel model: ShortcutsInstanceModel {
        id: shortcutsModel

        onShortcutsChanged: {
            for (var s = 0; s < root.objects.length; ++s) {
                root.objects[s].enabled = false
                root.objects[s].destroy()
            }
            root.objects = []

            for (var i = 0; i < shortcutsModel.shortcuts.length; ++i) {
                var sh = shortcutsModel.shortcuts[i]
                var obj = shortcutComponent.createObject(root, {sequence: sh})
                root.objects.push(obj)
            }
        }
    }

    property Component component: Component {
        id: shortcutComponent
        Shortcut {
            context: Qt.ApplicationShortcut
            enabled: shortcutsModel.active
            onActivated: shortcutsModel.activate(sequence)
        }
    }
}
