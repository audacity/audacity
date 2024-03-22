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
import QtQuick.Window 2.15

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0

MenuView {
    id: root

    property alias model: view.model

    property int preferredAlign: Qt.AlignRight // Left, HCenter, Right

    signal handleMenuItem(string itemId)

    property alias width: content.width
    property alias height: content.height

    signal loaded()

    function requestFocus() {
        var focused = prv.focusOnSelected()
        if (!focused) {
            focused = prv.focusOnFirstEnabled()
        }

        return focused
    }

    function calculateSize() {
        var menuMetricsComponent = Qt.createComponent("MenuMetrics.qml");
        root.menuMetrics = menuMetricsComponent.createObject(root)
        root.menuMetrics.calculate(model)

        //! NOTE: Due to the fact that the view has a dynamic delegate,
        //  the height calculation occurs with an error
        //  (by default, the delegate height is taken as the menu item height).
        //  Let's manually adjust the height of the content
        var sepCount = 0
        for (let i = 0; i < model.length; i++) {
            let item = Boolean(model.get) ? model.get(i).itemRole : model[i]
            if (!Boolean(item.title)) {
                sepCount++
            }
        }

        var itemHeight = 0
        for(var child in view.contentItem.children) {
            itemHeight = Math.max(itemHeight, view.contentItem.children[child].height)
        }

        var itemsCount = model.length - sepCount

        var anchorItemHeight = root.anchorGeometry().height

        root.contentWidth = root.menuMetrics.itemWidth
        root.contentHeight = Math.min(itemHeight * itemsCount + sepCount * prv.separatorHeight +
                                      prv.viewVerticalMargin * 2, anchorItemHeight - padding * 2)

        x = 0
        y = parent.height
    }

    onAboutToClose: function(closeEvent) {
        closeSubMenu()
    }

    function closeSubMenu() {
        if (root.subMenuLoader.isMenuOpened) {
            root.subMenuLoader.close()
        }
    }

    property var subMenuLoader: null
    property MenuMetrics menuMetrics: null

    contentItem: PopupContent {
        id: content

        property alias cascadeAlign: root.cascadeAlign

        objectName: "Menu"

        contentWidth: root.contentWidth
        contentHeight: root.contentHeight

        padding: root.padding
        margins: 0

        showArrow: root.showArrow
        opensUpward: root.opensUpward
        isOpened: root.isOpened

        animationEnabled: false //! NOTE disabled - because trouble with simultaneous opening of submenu
        closeOnEscape: false

        navigationSection.onNavigationEvent: function(event) {
            if (event.type === NavigationEvent.Escape) {
                if (root.subMenuLoader.isMenuOpened) {
                    root.subMenuLoader.close()
                } else {
                    root.close()
                }
            }
        }

        property NavigationPanel navigationPanel: NavigationPanel {
            name: "StyledMenu"
            section: content.navigationSection
            direction: NavigationPanel.Vertical
            order: 1

            onNavigationEvent: function(event) {
                switch (event.type) {
                case NavigationEvent.Right:
                    var selectedItem = prv.selectedItem()
                    if (!Boolean(selectedItem) || !selectedItem.hasSubMenu) {
                        return
                    }

                    //! NOTE Go to submenu if shown
                    selectedItem.openSubMenuRequested(false)

                    event.accepted = true

                    break
                case NavigationEvent.Left:
                    if (root.subMenuLoader.isMenuOpened) {
                        root.subMenuLoader.close()
                        event.accepted = true
                        return
                    }

                    //! NOTE Go to parent item
                    if (root.navigationParentControl) {
                        root.navigationParentControl.requestActive()
                    }

                    root.close()
                    break
                case NavigationEvent.Up:
                case NavigationEvent.Down:
                    if (root.subMenuLoader.isMenuOpened) {
                        root.subMenuLoader.close()
                    }

                    break
                }
            }
        }

        onCloseRequested: {
            root.close()
        }

        Component.onCompleted: {
            var menuLoaderComponent = Qt.createComponent("../StyledMenuLoader.qml");
            root.subMenuLoader = menuLoaderComponent.createObject(root)
            root.subMenuLoader.menuAnchorItem = root.anchorItem

            root.subMenuLoader.handleMenuItem.connect(function(itemId) {
                Qt.callLater(root.handleMenuItem, itemId)
                root.subMenuLoader.close()
            })

            root.subMenuLoader.opened.connect(function(itemId) {
                root.closePolicies = PopupView.NoAutoClose
            })

            root.subMenuLoader.closed.connect(function(force) {
                root.closePolicies = PopupView.CloseOnPressOutsideParent

                if (force) {
                    root.close(true)
                }
            })
        }

        StyledListView {
            id: view

            anchors.fill: parent
            anchors.topMargin: prv.viewVerticalMargin
            anchors.bottomMargin: prv.viewVerticalMargin

            spacing: 0
            interactive: contentHeight > root.height
            arrowControlsAvailable: true

            QtObject {
                id: prv

                readonly property int separatorHeight: 1
                readonly property int viewVerticalMargin: root.viewVerticalMargin()

                function focusOnFirstEnabled() {
                    for (var i = 0; i < view.count; ++i) {
                        var loader = view.itemAtIndex(i)
                        if (loader && !loader.isSeparator && loader.item && loader.item.enabled) {
                            loader.item.navigation.requestActive()
                            return true
                        }
                    }

                    return false
                }

                function focusOnSelected() {
                    var item = selectedItem()
                    if (Boolean(item)) {
                        item.navigation.requestActive()
                        return true
                    }

                    return false
                }

                function selectedItem() {
                    for (var i = 0; i < view.count; ++i) {
                        var loader = view.itemAtIndex(i)
                        if (loader && !loader.isSeparator && loader.item && loader.item.isSelected) {
                            return loader.item
                        }
                    }

                    return null
                }
            }

            delegate: Loader {
                id: loader

                property var itemData: Boolean(root.model.get) ? model.itemRole : modelData
                property bool isSeparator: !Boolean(itemData) || !Boolean(itemData.title) || itemData.title === ""

                sourceComponent: isSeparator ? separatorComp : menuItemComp

                onLoaded: {
                    loader.item.modelData = Qt.binding(() => (itemData))
                    loader.item.width = Qt.binding(() => ( Boolean(root.menuMetrics) ? root.menuMetrics.itemWidth : 0 ))
                    if (Boolean(loader.item.navigation)) {
                        loader.item.navigation.panel = content.navigationPanel
                    }
                }

                Component {
                    id: menuItemComp

                    StyledMenuItem {
                        id: item

                        property string title: Boolean (loader.itemData) ? loader.itemData.title : ""

                        menuAnchorItem: root.anchorItem
                        parentWindow: root.window

                        navigation.panel: content.navigationPanel
                        navigation.row: model.index

                        iconAndCheckMarkMode: Boolean(root.menuMetrics) ? root.menuMetrics.iconAndCheckMarkMode : StyledMenuItem.None

                        reserveSpaceForShortcutsOrSubmenuIndicator: Boolean(root.menuMetrics) ?
                                                                        (root.menuMetrics.hasItemsWithShortcut || root.menuMetrics.hasItemsWithSubmenu) : false

                        padding: root.padding

                        subMenuShowed: root.subMenuLoader.isMenuOpened && root.subMenuLoader.parent === item

                        onOpenSubMenuRequested: function(byHover) {
                            if (!hasSubMenu) {
                                if (byHover) {
                                    root.subMenuLoader.close()
                                }

                                return
                            }

                            if (!byHover) {
                                if (subMenuShowed) {
                                    root.subMenuLoader.close()
                                    return
                                }
                            }

                            root.subMenuLoader.parent = item
                            root.subMenuLoader.open(subMenuItems)
                        }

                        onCloseSubMenuRequested: {
                            root.subMenuLoader.close()
                        }

                        onHandleMenuItem: function(itemId) {
                            // NOTE: reset view state
                            view.update()

                            root.handleMenuItem(itemId)
                        }
                    }
                }

                Component {
                    id: separatorComp

                    Rectangle {
                        height: prv.separatorHeight
                        color: ui.theme.strokeColor

                        property var modelData
                    }
                }
            }
        }
    }
}
