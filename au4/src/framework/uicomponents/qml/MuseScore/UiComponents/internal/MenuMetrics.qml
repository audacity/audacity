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
import QtQml 2.15

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0

QtObject {
    id: root

    property bool hasItemsWithIconAndCheckable: false
    property bool hasItemsWithIconOrCheckable: false
    property bool hasItemsWithSubmenu: false
    property bool hasItemsWithShortcut: false

    property int itemLeftPartWidth: 100
    property int itemRightPartWidth: 100
    property int minimumMenuWidth: 178

    readonly property int itemWidth:
        Math.max(itemLeftPartWidth + itemRightPartWidth, minimumMenuWidth)

    property int iconAndCheckMarkMode: {
        if (hasItemsWithIconAndCheckable) {
            return StyledMenuItem.ShowBoth
        } else if (hasItemsWithIconOrCheckable) {
            return StyledMenuItem.ShowOne
        }
        return StyledMenuItem.None
    }

    function calculate(model) {
        root.hasItemsWithIconAndCheckable = false
        root.hasItemsWithIconOrCheckable = false
        root.hasItemsWithSubmenu = false
        root.hasItemsWithShortcut = false

        //! NOTE Policy:
        //! - if the menu contains checkable items, space for the checkmarks is reserved
        //! - if the menu contains items with an icon, space for icons is reserved
        //! - selectable items that don't have an icon are treated as checkable
        //! - selectable items that do have an icon are treated as non-checkable
        //! - all selectable items that are selected get an accent color background

        for (let i = 0; i < model.length; i++) {
            let item = Boolean(model.get) ? model.get(i).itemRole : model[i]
            let hasIcon = (Boolean(item.icon) && item.icon !== IconCode.NONE)

            if (item.checkable && hasIcon) {
                root.hasItemsWithIconAndCheckable = true
                root.hasItemsWithIconOrCheckable = true
            } else if (item.checkable || hasIcon || item.selectable) {
                root.hasItemsWithIconOrCheckable = true
            }

            if (Boolean(item.subitems) && item.subitems.length > 0) {
                root.hasItemsWithSubmenu = true
            }

            if (Boolean(item.shortcuts)) {
                root.hasItemsWithShortcut = true
            }
        }

        let leftWidth = 0
        let rightWidth = 0

        for (let j = 0; j < model.length; j++) {
            testItem.modelData = Boolean(model.get) ? model.get(j).itemRole : model[j]
            leftWidth = Math.max(leftWidth, testItem.calculatedLeftPartWidth())
            rightWidth = Math.max(rightWidth, testItem.calculatedRightPartWidth())
        }

        root.itemLeftPartWidth = leftWidth
        root.itemRightPartWidth = rightWidth
    }

    property StyledMenuItem testItem: StyledMenuItem {
        iconAndCheckMarkMode: root.iconAndCheckMarkMode

        reserveSpaceForShortcutsOrSubmenuIndicator:
            root.hasItemsWithShortcut || root.hasItemsWithSubmenu
    }
}
