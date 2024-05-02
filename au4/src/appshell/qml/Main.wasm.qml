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
import QtQuick 2.0
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.15
import MuseScore.NotationScene 1.0
import MuseScore.Playback 1.0
import Muse.UiComponents 1.0
import "DevTools/Audio"

ApplicationWindow {
    id: window
    width: 640
    height: 480

    visible: true
    title: qsTrc("appshell", "MuseScore")

    header: ToolBar {
        contentHeight: 40

        PlaybackToolBar {
            id: playbackToolbar
            anchors.top: parent.top
            anchors.bottom: parent.bottom
            anchors.left: parent.left
            color: ui.theme.backgroundPrimaryColor
        }

        Label {
            text: "position: "
            anchors.centerIn: parent
        }
    }


    RowLayout {
        anchors.fill: parent


        NotationView {
            id: notationView
            Layout.fillWidth: true
            Layout.fillHeight: true
        }

    }

}
