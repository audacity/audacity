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
import QtQuick.Controls 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Audacity.AppShell 1.0

Item {
    id: root

    width: radioButtonList.width
    height: radioButtonList.height

    property alias navigation: navPanel

    property string currentUri: "musescore://home"

    signal selected(string uri)

    function select(uri) {
        root.selected(uri)
    }

    function focusOnFirst() {
        var btn = radioButtonList.itemAtIndex(0)
        if (btn) {
            btn.navigation.requestActive()
        }
    }

    MainToolBarModel {
        id: toolBarModel
    }

    Component.onCompleted: {
        toolBarModel.load()
    }

    NavigationPanel {
        id: navPanel
        name: "MainToolBar"
        enabled: root.enabled && root.visible
        accessible.name: qsTrc("appshell", "Main toolbar") + " " + navPanel.directionInfo
    }

    RadioButtonGroup {
        id: radioButtonList
        spacing: 0

        model: toolBarModel

        width: contentItem.childrenRect.width
        height: contentItem.childrenRect.height

        delegate: PageTabButton {
            id: radioButtonDelegate

            ButtonGroup.group: radioButtonList.radioButtonGroup

            spacing: 0
            leftPadding: 12

            normalStateFont: model.isTitleBold ? ui.theme.largeBodyBoldFont : ui.theme.largeBodyFont

            navigation.name: model.title
            navigation.panel: navPanel
            navigation.order: model.index

            checked: model.uri === root.currentUri
            title: model.title

            onToggled: {
                root.selected(model.uri)
            }
        }
    }
}
