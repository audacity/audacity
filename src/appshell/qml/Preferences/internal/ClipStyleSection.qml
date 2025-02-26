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
import Muse.GraphicalEffects

import Audacity.ProjectScene

import "../../shared/internal"

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Clip style")

    property int imgWidth: 196
    property int imgHeight: 88
    property int imgRadius: 5
    property int currentClipStyle: ClipStyle.COLORFUL

    navigation.direction: NavigationPanel.Horizontal

    signal clipStyleChangeRequested(int style)

    Row {
        spacing: 16

        Column {
            spacing: 8

            ClipStyleSample {
                width: root.imgWidth
                height: root.imgHeight

                radius: root.imgRadius

                source: "qrc:/resources/Colorful.svg"

                onClipStyleChangeRequested: {
                    root.clipStyleChangeRequested(ClipStyle.COLORFUL)
                }
            }

            RoundedRadioButton {
                text: qsTrc("appshell/preferences", "Colorful")

                checked: root.currentClipStyle == ClipStyle.COLORFUL

                navigation.name: "ColorfulBox"
                navigation.panel: root.navigation
                navigation.row: 0

                onToggled: {
                    root.clipStyleChangeRequested(ClipStyle.COLORFUL)
                }
            }
        }

        Column {
            spacing: 8

            ClipStyleSample {
                width: root.imgWidth
                height: root.imgHeight

                radius: root.imgRadius

                source: "qrc:/resources/Classic.svg"

                onClipStyleChangeRequested: {
                    root.clipStyleChangeRequested(ClipStyle.CLASSIC)
                }
            }

            RoundedRadioButton {
                text: qsTrc("appshell/preferences", "Classic")

                checked: root.currentClipStyle == ClipStyle.CLASSIC

                navigation.name: "ClassicBox"
                navigation.panel: root.navigation
                navigation.row: 1

                onToggled: {
                    root.clipStyleChangeRequested(ClipStyle.CLASSIC)
                }
            }
        }
    }
}

