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

FocusScope {
    id: root

    property alias contentItem: contentLoader.item
    property alias contentItemComponent: contentLoader.sourceComponent
    property alias menuItemComponent: expandableSection.menuItemComponent

    property alias title: expandableSection.title
    property alias titleFont: expandableSection.titleFont

    property alias isExpanded: expandableSection.isExpanded

    property alias navigation: expandableSection.navigation

    implicitHeight: contentColumn.height
    implicitWidth: parent.width

    Keys.onSpacePressed: {
        root.isExpanded = !root.isExpanded
    }

    Column {
        id: contentColumn

        width: root.width

        spacing: 12

        ExpandableBlankSection {
            id: expandableSection
        }

        Loader {
            id: contentLoader

            property alias yScale: scalingFactor.yScale

            height: root.isExpanded ? implicitHeight : 0
            width: root.width

            enabled: root.isExpanded

            opacity: 0

            transform: Scale {
                id: scalingFactor

                yScale: 1
            }
        }
    }

    states: [
        State {
            name: "EXPANDED"
            when: root.isExpanded

            PropertyChanges { target: contentLoader; opacity: 1.0; yScale: 1 }
        },

        State {
            name: "COLLAPSED"
            when: !root.isExpanded

            PropertyChanges { target: contentLoader; opacity: 0.0; yScale: 0 }
        }
    ]

    transitions: Transition {
        NumberAnimation {
            properties: "opacity"
            duration: 100
        }
    }
}
