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
import QtQml 2.14
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0

Container {
    id: root

    property var buttons: []
    property alias buttonLayout: buttonBoxModel.buttonLayout

    implicitWidth: Math.max(implicitBackgroundWidth + leftInset + rightInset,
                            contentWidth + leftPadding + rightPadding)
    implicitHeight: Math.max(implicitBackgroundHeight + topInset + bottomInset,
                             contentHeight + topPadding + bottomPadding)

    padding: 0

    property bool isAccessibilityDisabledWhenInit: false
    property NavigationPanel navigationPanel: NavigationPanel {
        name: "ButtonBox"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Horizontal

        onNavigationEvent: function(event) {
            if (event.type === NavigationEvent.AboutActive) {
                var btn = root.firstFocusBtn
                if (Boolean(btn) && btn.enabled) {
                    event.setData("controlIndex", [ btn.navigation.row, btn.navigation.column ])
                }
            }
        }
    }


    property var firstFocusBtn: {
        var btn = accentButton()
        if (!Boolean(btn)) {
            btn = root.itemAt(0)
        }

        return btn
    }

    signal standardButtonClicked(int buttonId)

    function accentButton() {
        for (var i = 0; i < root.count; i++) {
            var btn = root.itemAt(i)
            if (btn.accentButton) {
                return btn
            }
        }

        return null
    }

    function addButton(text, buttonId, buttonRole, isAccent, isLeftSide) {
        const button = Qt.createQmlObject('
                                    import MuseScore.UiComponents 1.0
                                    FlatButton {
                                    }', root)
        button.text = text
        button.accentButton = isAccent

        button.buttonId = buttonId
        button.buttonRole = buttonRole
        button.isLeftSide = isLeftSide

        button.navigation.panel = root.navigationPanel
        button.navigation.column = root.count
        button.accessible.ignored = root.isAccessibilityDisabledWhenInit

        root.addItem(button)

        return button
    }

    function restoreAccessibility() {
        for (var i = 0; i < root.count; i++) {
            var btn = root.itemAt(i)
            if (!Boolean(btn.navigation)) {
                continue
            }

            btn.accessible.ignored = false
        }
    }

    contentItem: RowLayout {
        spacing: prv.spacing
        Repeater {
            model: root.contentModel
        }
    }

    ButtonBoxModel {
        id: buttonBoxModel

        buttonsItems: root.contentChildren
    }

    Connections {
        target: buttonBoxModel

        function onAddButton(text, buttonId, buttonRole, isAccent, isLeftSide) {
            const button = root.addButton(text, buttonId, buttonRole, isAccent, isLeftSide)

            button.clicked.connect(function() {
                root.standardButtonClicked(buttonId)
            })
        }

        function onReloadRequested() {
            prv.layoutButtons()
        }
    }

    Component.onCompleted: {
        buttonBoxModel.setButtons(root.buttons)
        prv.layoutButtons()
    }

    QtObject {
        id: prv

        property int spacing: 12

        function layoutButtons() {
            var buttonsTypes = buttonBoxModel.load()

            removeSeparator()

            var lastLeftSideButtonType = -1
            var buttonsWidths = 0

            for (var i = buttonsTypes.length - 1; i >= 0; i--) {
                var buttonInfo = prv.buttonInfo(buttonsTypes[i])
                if (!Boolean(buttonInfo)) {
                    continue
                }

                var btn = root.itemAt(i)
                if (buttonInfo.button !== btn) {
                    root.moveItem(buttonInfo.index, i)
                }

                buttonInfo.button.navigation.panel = root.navigationPanel
                buttonInfo.button.navigation.column = i

                //! NOTE See description about AccessibleItem { id: accessibleInfo }
                buttonInfo.button.accessible.ignored = root.isAccessibilityDisabledWhenInit
                const _buttonId = buttonInfo.button.buttonId

                if (lastLeftSideButtonType === -1 && buttonInfo.button.isLeftSide) {
                    lastLeftSideButtonType = buttonsTypes[i]
                }

                if (buttonInfo.button.visible) {
                    buttonsWidths += buttonInfo.button.width
                }
            }

            if (buttonsWidths + buttonsTypes.length * prv.spacing > root.width) {
                return
            }

            insertSeparator(lastLeftSideButtonType)
        }

        function buttonInfo(buttonType) {
            for (var i = 0; i < root.count; i++) {
                var btn = root.itemAt(i)
                if (!Boolean(btn)) {
                    continue
                }

                if (buttonType === btn.buttonId) {
                    return { index: i, button: btn }
                }
            }

            return null
        }

        function separatorInfo() {
            for (var i = 0; i < root.count; i++) {
                var item = root.itemAt(i)
                if (Boolean(item.isSeparator)) {
                    return { index: i, separator: item }
                }
            }

            return null
        }

        function removeSeparator() {
            var separatorInfo = prv.separatorInfo()
            if (Boolean(separatorInfo)) {
                root.removeItem(separatorInfo.index)
            }
        }

        function insertSeparator(lastLeftSideButtonType) {
            var index = 0
            if (lastLeftSideButtonType !== -1) {
                var lastLeftSideButtonInfo = buttonInfo(lastLeftSideButtonType)
                if (Boolean(lastLeftSideButtonInfo)) {
                    index = lastLeftSideButtonInfo.index + 1
                }
            }

            var separator = separatorComp.createObject(root)
            root.addItem(separator)

            for (var i = root.count - 2; i >= 0; i--) {
                var button = root.itemAt(i)
                if (button.buttonId === lastLeftSideButtonType) {
                    break;
                }

                root.moveItem(i, i + 1)
            }
        }
    }

    Component {
        id: separatorComp

        Item {
            property bool isSeparator: true
            property string text: "separator"
            Layout.fillWidth: true
            Layout.fillHeight: true
        }
    }
}
