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

import Audacity.Vst 1.0

Rectangle {

    // in
    property alias instanceId: view.instanceId

    // out
    property alias title: view.title

    color: ui.theme.backgroundPrimaryColor

    implicitWidth: view.implicitWidth
    implicitHeight: view.implicitHeight

    Component.onCompleted: {
        view.init()
    }

    function onApply() {
        viewModel.onApply()
    }

    function preview() {
       viewModel.preview()
    }

    VstViewModel {
        id: viewModel
        instanceId: view.instanceId
    }

    VstView {
        id: view
        anchors.fill: parent
    }
}
