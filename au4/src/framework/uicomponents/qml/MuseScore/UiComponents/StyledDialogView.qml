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
import MuseScore.UiComponents 1.0

DialogView {
    id: root

    default property alias contentData: contentBody.data

    property alias background: contentBackground

    property alias width: rootContainer.width
    property alias height: rootContainer.height

    property int margins: 0

    property alias navigationSection: navSec

    property bool closeOnEscape : true

    contentWidth: 240
    contentHeight: contentBody.childrenRect.height

    onOpened: {
        navSec.requestActive()
        root.navigationActivateRequested()
        accessibilityActiveTimer.start()
    }

    signal navigationActivateRequested()
    signal accessibilityActivateRequested()

    property Timer accessibilityActiveTimer: Timer {
        interval: 500
        repeat: false

        onTriggered: {
            root.accessibilityActivateRequested()
        }
    }

    contentItem: FocusScope {
        id: rootContainer
        width: contentBody.width + root.margins * 2
        height: contentBody.height + root.margins * 2

        implicitWidth: contentBody.implicitWidth + root.margins * 2
        implicitHeight: contentBody.implicitHeight + root.margins * 2

        //! NOTE: must to be inside QQuickItem to define a window by parent
        NavigationSection {
            id: navSec
            name: root.objectName !== "" ? root.objectName : "StyledDialogView"
            type: NavigationSection.Exclusive
            enabled: root.isOpened
            order: 1

            onActiveChanged: {
                if (navSec.active) {
                    rootContainer.forceActiveFocus()
                }
            }

            onNavigationEvent: function(event) {
                if (event.type === NavigationEvent.Escape && root.closeOnEscape) {
                    root.close()
                }
            }
        }

        StyledDropShadow {
            anchors.fill: parent
            source: contentBackground
            visible: root.frameless
        }

        Rectangle {
            id: contentBackground
            anchors.fill: parent
            color: ui.theme.backgroundPrimaryColor
            radius: root.frameless ? 4 : 0
            border.width: root.frameless ? 1 : 0
            border.color: ui.theme.strokeColor
        }

        Item {
            id: contentBody
            anchors.fill: parent
            anchors.margins: root.margins

            implicitWidth: root.contentWidth
            implicitHeight: root.contentHeight
        }
    }
}
