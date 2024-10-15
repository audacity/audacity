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
import Qt.labs.platform 1.1 as PLATFORM

import Audacity.AppShell 1.0

Item {
    readonly property bool available: menuModel.isGlobalMenuAvailable()

    PLATFORM.MenuBar {
        id: menuBar
    }

    PlatformAppMenuModel {
        id: menuModel
    }

    function load() {
        menuModel.load()

        var items = menuModel.items
        for (var i in items) {
            var item = items[i]
            var menu = makeMenu(item)

            menuBar.addMenu(menu)

            menu.load()

            item.subitemsChanged.connect(function(subitems, menuId) {
                for (var l in menuBar.menus) {
                    var menu = menuBar.menus[l]
                    if (menu.id === menuId) {
                        menu.subitems = subitems
                        menu.load()
                    }
                }
            })
        }

        menuModel.itemsChanged.connect(function() {
            for (var i in menuModel.items) {
                var menu = menuBar.menus[i]
                menu.subitems = menuModel.items[i].subitems
                menu.load()
            }
        })
    }

    function makeMenu(menuInfo) {
        var menu = menuComponent.createObject(menuBar)

        setUpMenu(menu, menuInfo)

        return menu
    }

    function setUpMenu(menu, menuInfo) {
        menu.id = menuInfo.id
        menu.title = menuInfo.title
        menu.enabled = menuInfo.enabled
        menu.subitems = menuInfo.subitems
    }

    function makeMenuItem(parentMenu, itemInfo) {
            var menuItem = menuItemComponent.createObject(parentMenu)

            setUpMenuItem(menuItem, itemInfo)

            return menuItem
    }

    function setUpMenuItem(menuItem, itemInfo) {
        menuItem.id = itemInfo.id
        menuItem.text = itemInfo.title + "\t" + itemInfo.portableShortcuts
        menuItem.enabled = itemInfo.enabled
        menuItem.checked = itemInfo.checked
        menuItem.checkable = itemInfo.checkable
        menuItem.separator = !Boolean(itemInfo.title)
        menuItem.role = itemInfo.role
    }

    Component {
        id: menuComponent

        PLATFORM.Menu {
            property string id: ""
            property var subitems: []

            function load() {
                clear()

                for (var i in subitems) {
                    var item = subitems[i]
                    if (!Boolean(item)) {
                        continue
                    }

                    var isMenu = Boolean(item.subitems) && item.subitems.length > 0

                    if (isMenu) {
                        let menu = makeMenu(item)
                        addMenu(menu)
                        menu.load()
                    } else {
                        let menuItem = makeMenuItem(this, item)
                        addItem(menuItem)
                    }
                }
            }

            function update() {
                for (var i in subitems) {
                    let item = subitems[i]
                    if (!Boolean(item)) {
                        continue
                    }

                    let isMenu = Boolean(item.subitems) && item.subitems.length > 0

                    if (isMenu) {
                        setUpMenu(items[i].subMenu, item)
                    } else {
                        setUpMenuItem(items[i], item)
                    }
                }
            }

            onAboutToShow: {
                update()
            }
        }
    }

    Component {
        id: menuItemComponent

        PLATFORM.MenuItem {
            property string id: ""

            onTriggered: {
                Qt.callLater(menuModel.handleMenuItem, id)
            }
        }
    }
}
