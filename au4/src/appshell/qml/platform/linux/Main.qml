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

import "../../"

AppWindow {
    id: root

    Loader {
        id: appMenuBar

        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right
    }

    Loader {
        id: platformMenuBar
    }

    Component.onCompleted: {
        platformMenuBar.setSource("../PlatformMenuBar.qml");
        if (platformMenuBar.item.available) {
            platformMenuBar.item.load();
            appMenuBar.active = 0;
        } else {
            appMenuBar.setSource("../AppMenuBar.qml", { "appWindow": root });
            platformMenuBar.active = 0;
        }

        window.init()
    }

    WindowContent {
        id: window

        anchors.top: appMenuBar.bottom
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
    }
}
