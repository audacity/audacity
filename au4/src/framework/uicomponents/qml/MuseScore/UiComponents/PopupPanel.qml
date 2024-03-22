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

import MuseScore.Ui 1.0
import MuseScore.GraphicalEffects 1.0

Rectangle {
    id: root

    property alias content: contentLoader.sourceComponent
    property alias background: effectSource.sourceItem
    property alias canClose: closeButton.visible

    property alias navigation: navPanel
    property NavigationControl navigationParentControl: null

    property alias accessible: navPanel.accessible

    signal opened()
    signal closed()

    color: ui.theme.popupBackgroundColor
    border.width: 1
    border.color: ui.theme.strokeColor

    radius: 20
    clip: true

    anchors.left: parent.left
    anchors.right: parent.right
    anchors.bottom: parent.bottom
    anchors.bottomMargin: -radius

    function setContentData(data) {
        if (contentLoader.status === Loader.Ready) {
            contentLoader.item.setData(data)
        }
    }

    function open(navigationCtrl) {
        if (navigationCtrl) {
            root.navigationParentControl = navigationCtrl
        }

        root.visible = true
        root.opened()
    }

    function close() {
        if (!root.canClose) {
            return
        }

        root.visible = false

        if (root.navigationParentControl) {
            root.navigationParentControl.requestActive()
        }

        root.closed()
    }

    property NavigationSection navigationSection: NavigationSection {
        name: root.objectName !== "" ? root.objectName : "PopupPanel"
        type: NavigationSection.Exclusive
        enabled: root.enabled && root.visible
        order: 1

        onActiveChanged: function(active) {
            if (active) {
                root.forceActiveFocus()
            }
        }

        onNavigationEvent: function(event) {
            if (event.type === NavigationEvent.Escape) {
                root.close()
            }
        }
    }

    NavigationPanel {
        id: navPanel
        name: root.objectName != "" ? root.objectName : "PopupPanel"

        enabled: root.visible
        section: root.navigationSection
        order: 2

        onActiveChanged: {
            if (navPanel.active) {
                root.forceActiveFocus()
            }
        }
    }

    ShaderEffectSource {
        id: effectSource
        anchors.fill: root

        height: root.height
        z: -1

        sourceRect: root.parent.mapToItem(sourceItem, root.x, root.y, root.width, root.height)

        Rectangle {
            anchors.fill: parent

            color: ui.theme.popupBackgroundColor
            opacity: 0.75
            radius: root.radius
        }
    }

    EffectFastBlur {
        anchors.fill: effectSource

        source: effectSource
        radius: 100
        transparentBorder: true
    }

    Loader {
        id: contentLoader
        anchors.fill: parent
        anchors.bottomMargin: -root.anchors.bottomMargin
    }

    FlatButton {
        id: closeButton

        anchors.top: parent.top
        anchors.topMargin: 20
        anchors.right: parent.right
        anchors.rightMargin: 20

        width: 30
        height: width

        icon: IconCode.CLOSE_X_ROUNDED
        transparent: true

        navigation.name: "CloseButton"
        navigation.panel: navPanel
        navigation.column: 1
        navigation.accessible.name: qsTrc("global", "Close")

        onClicked: {
            root.close()
        }
    }
}
