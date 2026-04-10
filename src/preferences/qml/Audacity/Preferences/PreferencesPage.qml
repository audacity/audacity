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
import Muse.UiComponents

Rectangle {
    id: root
    height: parent.height
    color: ui.theme.backgroundSecondaryColor

    default property alias contentData: content.data

    // If false, the contentHeight of the Flickable is determined by the height of the content items.
    // When necessary, the page becomes scrollable automatically.
    // If true, the contentHeight of the Flickable is determined by the available height for the page.
    property bool contentFillsAvailableHeight: false

    readonly property int sectionsSpacing: 24
    readonly property int sideMargin: 30

    property NavigationSection navigationSection: null
    property int navigationOrderStart: 0

    signal hideRequested()

    function apply() {
        return true
    }

    function reset() {
    }

    function ensureContentVisibleRequested(contentRect) {
        const overscroll = 60

        const { contentY, height, topMargin, bottomMargin, contentHeight } = flickable

        const visibleTop = contentY
        const visibleBottom = contentY + height - topMargin - bottomMargin

        const itemTop = contentRect.y
        const itemBottom = itemTop + contentRect.height

        const maxY = contentHeight - height + topMargin + bottomMargin
        const minY = -topMargin

        if (itemBottom > visibleBottom) {
            flickable.contentY = Math.min(itemBottom - height + topMargin + bottomMargin + overscroll, maxY)
        } else if (itemTop < visibleTop) {
            flickable.contentY = Math.max(itemTop - overscroll, minY)
        }
    }

    StyledFlickable {
        id: flickable
        anchors.fill: parent

        readonly property real availableWidth: width - leftMargin - rightMargin
        readonly property real availableHeight: height - topMargin - bottomMargin

        readonly property bool isScrollable: contentHeight > availableHeight

        contentWidth: availableWidth
        contentHeight: root.contentFillsAvailableHeight ? availableHeight : content.childrenRect.height

        topMargin: root.sideMargin
        leftMargin: root.sideMargin
        rightMargin: root.sideMargin
        bottomMargin: root.sideMargin

        ScrollBar.vertical: StyledScrollBar { id: scrollBar }

        Item {
            id: content
            anchors.fill: parent
        }
    }

    GradientRectangle {
        id: topGradient
        visible: flickable.isScrollable

        anchors.top: flickable.top
        anchors.left: flickable.left
        anchors.right: flickable.right
        anchors.rightMargin: scrollBar.width

        height: root.sideMargin

        startColor: root.color
        endColor: "transparent"
    }

    GradientRectangle {
        id: bottomGradient
        visible: flickable.isScrollable

        anchors.left: flickable.left
        anchors.right: flickable.right
        anchors.rightMargin: scrollBar.width
        anchors.bottom: flickable.bottom

        height: root.sideMargin

        startColor: "transparent"
        endColor: root.color
    }
}
