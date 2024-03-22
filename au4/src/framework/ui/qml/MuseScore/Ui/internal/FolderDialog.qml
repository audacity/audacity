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
import Qt.labs.platform 1.1 as QtPlatform

QtPlatform.FolderDialog {
    id: root

    property string objectId: ""
    property var ret: null

    signal opened()
    signal closed()

    onVisibleChanged: {
        if (visible) {
            root.opened()
        }
    }

    onAccepted: {
        root.ret = { "errcode": 0, "value":  root.currentFolder.toString() }
        root.close()
        root.closed()
    }

    onRejected: {
        root.ret = { "errcode": 3 }
        root.close()
        root.closed()
    }
}
