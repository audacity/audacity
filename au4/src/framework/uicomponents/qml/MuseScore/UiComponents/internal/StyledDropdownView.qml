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

DropdownView {
    id: root

    property var model: null
    required property int visibleItemsCount

    default property alias contentData: content.contentData

    property alias background: content.background

    property alias width: content.width
    property alias height: content.height

    property alias margins: content.margins

    property alias animationEnabled: content.animationEnabled

    property alias closeOnEscape: content.closeOnEscape
    property alias navigationSection: content.navigationSection

    required property int currentIndex
    required property string textRole
    required property string valueRole

    required property int itemWidth
    required property int itemHeight

    property int contentWidth: root.itemWidth
    property int contentHeight: content.contentBodyHeight

    //! NOTE: Due to the fact that the dropdown window opens without activating focus,
    //!       for all items in the dropdown, the accessible window must be the window
    //!       of the element from which the dropdown was opened
    required property var accessibleWindow

    x: 0
    y: 0

    padding: 8
    margins: 0

    showArrow: false

    openPolicies: PopupView.NoActivateFocus

    signal handleItem(int index, var value)

    onOpened: {
        content.navigationSection.requestActive()

        prv.positionViewAtIndex(root.currentIndex)
    }

    onClosed: {
        content.focus = false
    }

    onModelChanged: {
        root.contentHeight = root.itemHeight * Math.min(root.model.length, root.visibleItemsCount)

        var viewHeight = root.itemHeight * Math.min(model.length, root.visibleItemsCount)

        var viewCenterY = viewHeight / 2
        if (root.currentIndex === -1) {
            root.focusItemY = viewCenterY
            return
        }

        var itemY = root.currentIndex * root.itemHeight

        if (itemY < viewCenterY) {
            root.focusItemY = itemY
            return
        }

        var newViewHeight = (model.length - Math.min(model.length, root.visibleItemsCount)) * root.itemHeight + viewHeight
        var newViewCenterY = newViewHeight - viewHeight / 2

        if (itemY < newViewCenterY) {
            root.focusItemY = viewCenterY
            return
        }

        if (itemY > newViewCenterY) {
            root.focusItemY = viewHeight - (model.length - root.currentIndex) * root.itemHeight
            return
        }

        root.focusItemY = viewCenterY
    }

    contentItem: PopupContent {
        id: content

        objectName: "Dropdown"

        contentWidth: root.contentWidth
        contentHeight: root.contentHeight

        padding: root.padding

        showArrow: root.showArrow
        arrowX: root.arrowX
        opensUpward: root.opensUpward
        isOpened: root.isOpened

        onCloseRequested: {
            root.close()
        }

        StyledListView {
            id: view

            anchors.fill: parent

            model: root.model

            property NavigationPanel navigationPanel: NavigationPanel {
                name: "Dropdown"
                section: root.navigationSection
                direction: NavigationPanel.Vertical
                order: 1
                accessible.window: root.accessibleWindow

                onNavigationEvent: function(event) {
                    if (event.type === NavigationEvent.AboutActive) {
                        var item = view.itemAtIndex(root.currentIndex)
                        var controlName = item.navigation.name
                        if (controlName !== "") {
                            event.setData("controlName", controlName)
                        }
                    }
                }
            }

            QtObject {
                id: prv

                function itemIndexByFirstChar(text) {
                    if (text === "") {
                        return;
                    }

                    text = text.toLowerCase()
                    for (var i = 0; i < root.model.length; ++i) {
                        var itemText =  Utils.getItemValue(root.model, i, root.textRole, "")
                        if (itemText.toLowerCase().startsWith(text)) {
                            return i
                        }
                    }

                    return -1
                }

                function positionViewAtIndex(itemIndex) {
                    view.positionViewAtIndex(itemIndex, ListView.Contain)

                    correctPosition(itemIndex)
                }

                function correctPosition(itemIndex) {
                    var item = view.itemAtIndex(itemIndex)
                    if (Boolean(item)) {
                        var diff = item.mapToGlobal(0, 0).y - root.parent.mapToGlobal(0, 0).y

                        if (view.contentY + diff + view.height > view.contentHeight) {
                            view.positionViewAtEnd()
                        } else if (view.contentY + diff < 0) {
                            view.positionViewAtBeginning()
                        } else {
                            view.contentY += diff
                        }
                    }

                    Qt.callLater(navigateToItem, itemIndex)
                }

                function navigateToItem(itemIndex, byUser) {
                    var item = view.itemAtIndex(itemIndex)
                    if (Boolean(item)) {
                        item.navigation.requestActive(byUser)
                    }
                }
            }

            delegate: ListItemBlank {
                id: item

                objectName: "dropitem"

                height: root.itemHeight
                width: root.contentWidth

                normalColor: ui.theme.buttonColor
                radius: 0

                isSelected: model.index === root.currentIndex

                navigation.name: label.text
                navigation.panel: view.navigationPanel
                navigation.row: model.index
                navigation.accessible.name: label.text
                navigation.accessible.window: root.accessibleWindow
                navigation.onActiveChanged: {
                    if (navigation.highlight) {
                        view.positionViewAtIndex(model.index, ListView.Contain)
                    }
                }

                Keys.onShortcutOverride: function(event) {
                    if (event.text === "") {
                        event.accepted = false
                        return
                    }

                    if (prv.itemIndexByFirstChar(event.text) > -1) {
                        event.accepted = true
                    }
                }

                Keys.onPressed: function(event) {
                    if (event.text === "") {
                        return
                    }

                    var index = prv.itemIndexByFirstChar(event.text)
                    if (index > -1) {
                        view.positionViewAtIndex(index, ListView.Contain)
                        Qt.callLater(navigateToItem, index, true)
                    }
                }

                StyledTextLabel {
                    id: label
                    anchors.fill: parent
                    anchors.leftMargin: 12
                    horizontalAlignment: Text.AlignLeft

                    text: Utils.getItemValue(root.model, model.index, root.textRole, "")
                }

                onClicked: {
                    var value = Utils.getItemValue(root.model, model.index, root.valueRole, undefined)
                    root.handleItem(model.index, value)
                }

                mouseArea.onContainsMouseChanged: {
                    if (!label.truncated) {
                        return
                    }

                    if (mouseArea.containsMouse) {
                        ui.tooltip.show(item, label.text)
                    } else {
                        ui.tooltip.hide(item)
                    }
                }
            }
        }
    }
}
