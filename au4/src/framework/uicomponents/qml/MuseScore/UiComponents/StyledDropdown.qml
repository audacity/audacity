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

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0

import "internal"

Item {
    id: root

    property var model: null
    property int count: Boolean(model) ? model.length : 0
    property string textRole: "text"
    property string valueRole: "value"

    property int currentIndex: -1

    property string currentText: Utils.getItemValue(model, currentIndex, textRole, indeterminateText)
    property var currentValue: Utils.getItemValue(model, currentIndex, valueRole, undefined)

    property string displayText : root.currentText
    property string indeterminateText: "--"

    property int popupItemsCount: 18

    property alias dropIcon: dropIconItem
    property alias label: mainItem.label

    property alias navigation: mainItem.navigation

    signal activated(int index, var value)

    height: 30
    width: 126

    function indexOfValue(value) {
        if (!root.model) {
            return -1
        }

        for (var i = 0; i < root.count; ++i) {
            if (Utils.getItemValue(root.model, i, root.valueRole) === value) {
                return i
            }
        }

        return -1
    }

    function indexOfText(text) {
        if (!root.model) {
            return -1
        }

        for (var i = 0; i < root.count; ++i) {
            if (Utils.getItemValue(root.model, i, root.textRole) === text) {
                return i
            }
        }

        return -1
    }

    function textOfValue(value) {
        if (!root.model) {
            return ""
        }

        for (var i = 0; i < root.count; ++i) {
            if (Utils.getItemValue(root.model, i, root.valueRole) === value) {
                return Utils.getItemValue(model, i, textRole, indeterminateText)
            }
        }

        return ""
    }

    function ensureActiveFocus() {
        if (mainItem.navigation) {
            mainItem.navigation.requestActive()
        }
    }

    DropdownItem {
        id: mainItem

        anchors.fill: parent

        text: root.displayText

        background.border.width: ui.theme.borderWidth
        background.border.color: ui.theme.strokeColor

        navigation.accessible.role: MUAccessible.ComboBox

        onClicked: {
            dropdownLoader.toggleOpened(root.model)
        }

        StyledDropdownLoader {
            id: dropdownLoader

            textRole: root.textRole
            valueRole: root.valueRole

            currentIndex: root.currentIndex

            itemWidth: mainItem.width
            itemHeight: mainItem.height

            visibleItemsCount: root.popupItemsCount

            onHandleItem: function(index, value) {
                root.activated(index, value)
            }
        }
    }

    StyledIconLabel {
        id: dropIconItem
        anchors.verticalCenter: parent.verticalCenter
        anchors.right: parent.right
        anchors.rightMargin: 8

        iconCode: IconCode.SMALL_ARROW_DOWN
    }
}
