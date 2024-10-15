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
import QtQuick.Window 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import MuseScore.GraphicalEffects 1.0
import Audacity.AppShell 1.0

Page {
    title: qsTrc("appshell/gettingstarted", "Video tutorials")
    explanation: qsTrc("appshell/gettingstarted", "Behind this dialog is the ‘Learn’ section, where you’ll find tutorials to get you started\n(Video tutorials require an internet connection)")

    titleContentSpacing: 12

    Image {
        id: image
        anchors.fill: parent
        fillMode: Image.PreserveAspectCrop
        source: "resources/VideoTutorials.png"
        sourceSize: Qt.size(width * Screen.devicePixelRatio, height * Screen.devicePixelRatio)

        layer.enabled: true
        layer.effect: EffectOpacityMask {
            maskSource: Rectangle {
                width: image.width
                height: image.height
                radius: 3
            }
        }
    }
}
