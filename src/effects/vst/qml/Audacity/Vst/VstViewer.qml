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

import Audacity.Effects 1.0
import Audacity.Vst 1.0

Rectangle {

    id: root

    // in
    property alias instanceId: view.instanceId

    // out
    property alias title: view.title

    color: ui.theme.backgroundPrimaryColor

    implicitWidth: view.implicitWidth
    implicitHeight: view.implicitHeight

    Component.onCompleted: {
        viewModel.init()
        view.init()
        Qt.callLater(manageMenuModel.load)
    }

    function preview() {
       viewModel.preview()
    }

    function manage(parent) {
        var px = parent.x
        var py = parent.y + parent.height
        var pos = mapFromItem(parent, px, py)

        menuLoader.show(pos, manageMenuModel)
    }

    EffectManageMenu {
        id: manageMenuModel
        instanceId: view.instanceId
    }

    ContextMenuLoader {
        id: menuLoader

        onHandleMenuItem: function(itemId) {
            manageMenuModel.handleMenuItem(itemId)
        }
    }

    VstViewModel {
        id: viewModel
        instanceId: view.instanceId
    }

    VstView {
        id: view
        width: implicitWidth
        height: implicitHeight
        x: (root.width - view.width) / 2
    }
}
