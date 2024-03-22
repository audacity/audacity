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
import QtQuick.Layouts 1.15

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0

ListItemBlank {
    id: root

    property var modelData: null
    property var menuAnchorItem: null

    property var parentWindow: null

    enum IconAndCheckMarkMode {
        None,
        ShowOne,
        ShowBoth
    }

    property int iconAndCheckMarkMode: StyledMenuItem.ShowOne
    property bool reserveSpaceForShortcutsOrSubmenuIndicator: itemPrv.hasShortcuts || root.hasSubMenu

    property int padding: 0

    property bool subMenuShowed: false
    property bool hasSubMenu: Boolean(modelData) && Boolean(modelData.subitems) && modelData.subitems.length > 0
    property var subMenuItems: hasSubMenu ? modelData.subitems : null

    signal handleMenuItem(string itemId)

    signal openSubMenuRequested(bool byHover)
    signal closeSubMenuRequested()
    signal subMenuShowed(var menu)
    signal subMenuClosed()

    signal requestParentItemActive()

    implicitHeight: 32

    hoverHitColor: ui.theme.accentColor
    enabled: (Boolean(modelData) && modelData.enabled !== undefined) ? Boolean(modelData.enabled) : true // default true

    isSelected: subMenuShowed || (itemPrv.isSelectable && itemPrv.isSelected) || navigation.highlight

    navigation.name: Boolean(itemPrv.id) ? itemPrv.id : titleLabel.text
    navigation.accessible.role: MUAccessible.MenuItem
    navigation.accessible.name: {
        var text = itemPrv.title
        if (itemPrv.isCheckable) {
            text += " " + (itemPrv.isChecked
                           //: Describes the 'on' state of a toggle-able ui item.
                           ? qsTrc("ui", "checked", "checkstate")
                           //: Describes the 'off' state of a toggle-able ui item.
                           : qsTrc("ui", "unchecked", "checkstate"))
        } else if (itemPrv.isSelectable) {
            text += " " + (itemPrv.isSelected
                           //: Describes the state of a ui item that is the chosen option in a list of options
                           ? qsTrc("ui", "selected", "selectedState")
                           //: Describes the state of a ui item that is not the chosen option in a list of options
                           : qsTrc("ui", "not selected", "selectedState"))
        }

        if (itemPrv.hasShortcuts) {
            text += " " + itemPrv.shortcuts
        }

        if (root.hasSubMenu) {
            //: a type of ui item
            text += " " + qsTrc("ui", "Menu")
        }

        return text
    }

    QtObject {
        id: itemPrv

        property string id: Boolean(modelData) && Boolean(modelData.id) ? modelData.id : ""

        property string title: Boolean(modelData) && Boolean(modelData.title) ? modelData.title : ""
        property string titleWithMnemonicUnderline: Boolean(modelData) && Boolean(modelData.titleWithMnemonicUnderline) ? modelData.titleWithMnemonicUnderline : title

        property bool hasShortcuts: Boolean(modelData) && Boolean(modelData.shortcuts)
        property string shortcuts: hasShortcuts ? modelData.shortcuts : ""

        property bool isCheckable: Boolean(modelData) && Boolean(modelData.checkable)
        property bool isChecked: isCheckable && Boolean(modelData.checked)

        property bool isSelectable: Boolean(modelData) && Boolean(modelData.selectable)
        property bool isSelected: isSelectable && Boolean(modelData.selected)

        property bool hasIcon: Boolean(modelData) && Boolean(modelData.icon) && modelData.icon !== IconCode.NONE
    }

    function calculatedLeftPartWidth() {
        let result = 0

        result += rowLayout.anchors.leftMargin
        if (primaryIconLabel.visible) {
            result += Math.ceil(primaryIconLabel.width)
            result += rowLayout.spacing
        }

        if (secondaryIconLabel.visible) {
            result += Math.ceil(secondaryIconLabel.width)
            result += rowLayout.spacing
        }

        result += Math.ceil(titleLabel.implicitWidth)
        result += rowLayout.spacing // Should theoretically not be necessary, but in practice it is

        return result
    }

    function calculatedRightPartWidth() {
        let result = 0

        if (shortcutsLabel.visible) {
            result += rowLayout.spacing
            result += Math.ceil(shortcutsLabel.width)
        }

        if (submenuIndicator.visible) {
            result += rowLayout.spacing
            result += Math.ceil(submenuIndicator.width)
        }

        result += rowLayout.anchors.rightMargin

        return result
    }

    RowLayout {
        id: rowLayout

        anchors.verticalCenter: parent.verticalCenter
        anchors.left: parent.left
        anchors.leftMargin: 12
        anchors.right: parent.right
        anchors.rightMargin: submenuIndicator.visible ? 6 : 12

        spacing: 12

        StyledIconLabel {
            id: primaryIconLabel
            Layout.alignment: Qt.AlignLeft
            width: 16
            iconCode: {
                if (root.iconAndCheckMarkMode !== StyledMenuItem.ShowBoth && itemPrv.hasIcon) {
                    return itemPrv.hasIcon ? modelData.icon : IconCode.NONE
                } else if (itemPrv.isCheckable) {
                    return itemPrv.isChecked ? IconCode.TICK_RIGHT_ANGLE : IconCode.NONE
                } else  if (itemPrv.isSelectable) {
                    return itemPrv.isSelected ? IconCode.TICK_RIGHT_ANGLE : IconCode.NONE
                }

                return IconCode.NONE
            }
            visible: !isEmpty || root.iconAndCheckMarkMode !== StyledMenuItem.None
        }

        StyledIconLabel {
            id: secondaryIconLabel
            Layout.alignment: Qt.AlignLeft
            width: 16
            iconCode: itemPrv.hasIcon ? modelData.icon : IconCode.NONE
            visible: root.iconAndCheckMarkMode === StyledMenuItem.ShowBoth
        }

        StyledTextLabel {
            id: titleLabel
            Layout.fillWidth: true
            horizontalAlignment: Text.AlignLeft

            text: itemPrv.titleWithMnemonicUnderline

            textFormat: Text.RichText
            //! If the rich text format is set, then the component intercepts the hover state
            //  The hover state is required to open a submenu(see onHovered)
            //  So, let's turn off the mouse hovering over the component
            enabled: false
            opacity: root.enabled ? 1.0 : ui.theme.itemOpacityDisabled
        }

        StyledTextLabel {
            id: shortcutsLabel
            Layout.alignment: Qt.AlignRight
            text: itemPrv.shortcuts
            horizontalAlignment: Text.AlignRight
            visible: !itemPrv.hasShortcuts || (root.reserveSpaceForShortcutsOrSubmenuIndicator)
        }

        StyledIconLabel {
            id: submenuIndicator
            Layout.alignment: Qt.AlignRight
            width: 16
            iconCode: root.hasSubMenu ? IconCode.SMALL_ARROW_RIGHT : IconCode.NONE
            visible: !isEmpty || (root.reserveSpaceForShortcutsOrSubmenuIndicator && !shortcutsLabel.visible)
        }
    }

    onHovered: function(isHovered, mouseX, mouseY) {
        if (isHovered) {
            root.openSubMenuRequested(true)
        }
    }

    onClicked: {
        if (root.hasSubMenu) {
            root.openSubMenuRequested(false)
            return
        }

        root.handleMenuItem(modelData.id)
    }
}
