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

import "../"

Row {
    id: root

    property bool windowIsMiximized: false

    spacing: 8

    signal showWindowMinimizedRequested()
    signal toggleWindowMaximizedRequested()
    signal closeWindowRequested()

    FlatButton {
        id: minimizeButton

        icon: IconCode.APP_MINIMIZE
        transparent: true
        drawFocusBorderInsideRect: true

        backgroundItem: AppButtonBackground {
            mouseArea: minimizeButton.mouseArea
        }

        onClicked: {
            root.showWindowMinimizedRequested()
        }
    }

    FlatButton {
        id: maximizeButton

        icon: !root.windowIsMiximized ? IconCode.APP_MAXIMIZE : IconCode.APP_UNMAXIMIZE
        transparent: true
        drawFocusBorderInsideRect: true

        backgroundItem: AppButtonBackground {
            mouseArea: maximizeButton.mouseArea
        }

        onClicked: {
            root.toggleWindowMaximizedRequested()
        }
    }

    FlatButton {
        id: closeButton

        icon: IconCode.APP_CLOSE
        transparent: true
        drawFocusBorderInsideRect: true

        backgroundItem: AppButtonBackground {
            mouseArea: closeButton.mouseArea
        }

        onClicked: {
            root.closeWindowRequested()
        }
    }
}
