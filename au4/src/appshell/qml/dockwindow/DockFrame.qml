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

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Muse.Dock 1.0

Rectangle {
    id: root

    //! NOTE: please, don't rename those properties because they are used in c++
    property QtObject frameCpp
    readonly property QtObject titleBarCpp: Boolean(frameCpp) ? frameCpp.actualTitleBar : null
    readonly property int nonContentsHeight: titleBar.visible ? titleBar.heightWhenVisible + tabsPanel.height : 0
    property int titleBarNavigationPanelOrder: 1
    //! ---

    anchors.fill: parent
    color: ui.theme.backgroundPrimaryColor

    onFrameCppChanged: {
        if (Boolean(frameCpp)) {
            frameCpp.setStackLayout(stackLayout)
        }
    }

    onNonContentsHeightChanged: {
        if (Boolean(frameCpp)) {
            frameCpp.geometryUpdated()
        }
    }

    Component.onDestruction: {
        tabs.model = 0
    }

    DockFrameModel {
        id: frameModel

        frame: root.frameCpp
    }

    NavigationPanel {
        id: navPanel
        name: root.objectName+"PanelTabs"
        enabled: root.enabled && root.visible
        section: frameModel.navigationSection
        order: root.titleBarNavigationPanelOrder

        onNavigationEvent: function(event) {
            if (event.type === NavigationEvent.AboutActive && tabsPanel.visible) {
                event.setData("controlName", tabs.currentItem.navigation.name)
            }
        }
    }

    DockTitleBar {
        id: titleBar

        anchors.top: parent.top

        titleBarCpp: root.titleBarCpp

        contextMenuModel: frameModel.currentDockContextMenuModel
        visible: frameModel.titleBarVisible
        isHorizontalPanel: frameModel.isHorizontalPanel

        navigation.panel: navPanel
        navigation.order: 1

        onHandleContextMenuItemRequested: function(itemId) {
            frameModel.handleMenuItem(itemId)
        }
    }

    MouseArea {
        id: dragMouseArea

        anchors.top: tabsPanel.top
        width: tabs.contentWidth
        height: tabsPanel.height - bottomSeparator.height

        z: tabsPanel.z + 1

        hoverEnabled: false
        propagateComposedEvents: true
        enabled: tabsPanel.visible
        cursorShape: Qt.SizeAllCursor
    }

    Rectangle {
        id: tabsPanel

        anchors.top: titleBar.visible ? titleBar.bottom : parent.top

        height: visible ? 35 : 0
        width: parent.width

        visible: tabs.count > 1
        clip: true

        color: ui.theme.backgroundSecondaryColor

        readonly property QtObject tabBarCpp: Boolean(root.frameCpp) ? root.frameCpp.tabWidget.tabBar : null
        property int currentIndex: Boolean(root.frameCpp) && root.frameCpp.currentIndex >= 0 ? root.frameCpp.currentIndex : 0

        onTabBarCppChanged: {
            if (Boolean(tabBarCpp)) {
                tabBarCpp.setDraggableMouseArea(dragMouseArea)
                tabBarCpp.tabBarQmlItem = this
            }
        }

        onCurrentIndexChanged: {
            if (Boolean(root) && Boolean(root.frameCpp)) {
                root.frameCpp.tabWidget.setCurrentDockWidget(currentIndex)
            }
        }

        ListView {
            id: tabs

            anchors.top: parent.top
            anchors.bottom: parent.bottom
            anchors.left: parent.left

            width: Math.min(contentWidth, availableWidth)

            orientation: Qt.Horizontal
            interactive: false
            spacing: 0

            currentIndex: tabsPanel.currentIndex
            model: frameModel.tabs

            readonly property real availableWidth: tabsPanel.width + 1  // + 1, because we don't need to see the rightmost separator
            readonly property real implicitWidthOfActiveTab: currentItem ? currentItem.implicitWidth : 0
            readonly property real implicitWidthOfAllTabsTogether: {
                let result = 0
                let items = tabs.contentItem.children

                for (let i in items) {
                    let item = items[i]
                    if (item && item.implicitWidth) {
                        result += item.implicitWidth
                    }
                }

                return result
            }

            delegate: DockPanelTab {
                text: modelData.title
                isCurrent: tabsPanel && (tabsPanel.currentIndex === model.index)
                contextMenuModel: modelData.contextMenuModel

                width: isCurrent || (tabs.implicitWidthOfAllTabsTogether <= tabs.availableWidth)
                       ? implicitWidth
                       : (tabs.availableWidth - tabs.implicitWidthOfActiveTab)
                         / (tabs.implicitWidthOfAllTabsTogether - tabs.implicitWidthOfActiveTab)
                         * implicitWidth

                navigation.name: text
                navigation.panel: navPanel
                navigation.order: model.index * 2 // NOTE '...' button will have +1 order

                onNavigationTriggered: {
                    tabsPanel.currentIndex = model.index
                }

                onClicked: {
                    tabsPanel.currentIndex = model.index
                }

                onHandleContextMenuItemRequested: function(itemId) {
                    frameModel.handleMenuItem(itemId)
                }
            }
        }

        Item {
            anchors.left: tabs.right
            anchors.right: parent.right
            anchors.bottom: parent.bottom

            height: bottomSeparator.height

            SeparatorLine { id: bottomSeparator }
        }
    }

    StackLayout {
        id: stackLayout

        anchors.top: tabsPanel.visible ? tabsPanel.bottom : (titleBar.visible ? titleBar.bottom : parent.top)
        anchors.topMargin: tabsPanel.visible ? 12 : 0
        anchors.bottom: parent.bottom

        width: parent.width

        currentIndex: {
            var currentDockUniqueName = frameModel.currentDockUniqueName

            for (var i in children) {
                if (children[i].uniqueName === currentDockUniqueName) {
                    return i
                }
            }

            return 0
        }
    }

    Rectangle {
        visible: frameModel.highlightingVisible

        x: frameModel.highlightingRect.x
        y: frameModel.highlightingRect.y
        width: frameModel.highlightingRect.width
        height: frameModel.highlightingRect.height

        color: "transparent"

        border.width: 1
        border.color: ui.theme.accentColor

        Rectangle {
            anchors.fill: parent

            color: ui.theme.accentColor
            opacity: 0.3
        }
    }
}
