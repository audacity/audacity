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
import QtQuick.Layouts 1.3

import MuseScore.Ui 1.0
import MuseScore.UiComponents 1.0

FocusScope {
    id: root

    property bool isIndeterminate: false
    readonly property string indeterminateText: "--"
    property var currentText: ""
    property alias validator: valueInput.validator
    property alias maximumLength: valueInput.maximumLength
    property alias measureUnitsSymbol: measureUnitsLabel.text

    property alias hint: valueInput.placeholderText
    property alias hintIcon: hintIcon.iconCode
    property bool clearTextButtonVisible: false

    property alias textHorizontalAlignment: valueInput.horizontalAlignment
    property alias textVerticalAlignment: valueInput.verticalAlignment
    property bool hasText: valueInput.text.length > 0
    property alias readOnly: valueInput.readOnly

    property real textSidePadding: 12
    property real accessoriesPadding: 4

    readonly property alias background: background

    readonly property alias mouseArea: clickableArea
    property bool containsMouse: clickableArea.containsMouse

    readonly property alias navigation: navCtrl
    readonly property alias accessible: navCtrl.accessible

    readonly property alias clearTextButton: clearTextButtonItem

    signal textChanged(var newTextValue)
    signal textEdited(var newTextValue)
    signal textCleared()
    signal textEditingFinished(var newTextValue)
    signal accepted()
    signal escapted()

    function selectAll() {
        valueInput.selectAll()
    }

    function clear() {
        valueInput.text = ""
        currentText = ""
        textCleared()
    }

    function ensureActiveFocus() {
        if (!root.activeFocus) {
            root.forceActiveFocus()
        }
    }

    onActiveFocusChanged: {
        if (activeFocus) {
            valueInput.forceActiveFocus()
        }
    }

    implicitHeight: 30
    implicitWidth: parent.width

    opacity: root.enabled ? 1.0 : ui.theme.itemOpacityDisabled

    FocusListener {
        item: root
    }

    NavigationControl {
        id: navCtrl
        name: root.objectName !== "" ? root.objectName : "TextInputField"
        enabled: root.enabled && root.visible

        accessible.role: MUAccessible.EditableText
        accessible.name: Boolean(valueInput.text) ? valueInput.text + " " + measureUnitsLabel.text : valueInput.placeholderText
        accessible.visualItem: root
        accessible.text: valueInput.text
        accessible.selectedText: valueInput.selectedText
        accessible.selectionStart: valueInput.selectionStart
        accessible.selectionEnd: valueInput.selectionEnd
        accessible.cursorPosition: valueInput.cursorPosition

        onActiveChanged: {
            if (navCtrl.active) {
                root.ensureActiveFocus()
            }
        }
    }

    Rectangle {
        id: background
        anchors.fill: parent

        NavigationFocusBorder { navigationCtrl: navCtrl }

        color: ui.theme.textFieldColor
        border.color: ui.theme.strokeColor
        border.width: Math.max(ui.theme.borderWidth, 1)
        radius: 3
    }

    RowLayout {
        anchors.fill: parent
        anchors.leftMargin: hintIcon.visible ? 0 : root.textSidePadding
        anchors.rightMargin: clearTextButtonItem.visible ? 0 : root.textSidePadding

        spacing: 0

        StyledIconLabel {
            id: hintIcon

            Layout.fillHeight: true
            Layout.preferredWidth: height
            Layout.margins: root.accessoriesPadding

            visible: !isEmpty
        }

        TextField {
            id: valueInput

            objectName: "TextField"

            Layout.alignment: Qt.AlignVCenter
            Layout.fillWidth: !measureUnitsLabel.visible
            padding: 0

            color: ui.theme.fontPrimaryColor
            font: ui.theme.bodyFont

            background: Item {}

            focus: false
            activeFocusOnPress: false
            selectByMouse: true
            selectionColor: Utils.colorWithAlpha(ui.theme.accentColor, ui.theme.accentOpacityNormal)
            selectedTextColor: ui.theme.fontPrimaryColor
            placeholderTextColor: ui.theme.fontPrimaryColor
            visible: !root.isIndeterminate || activeFocus

            text: root.currentText === undefined ? "" : root.currentText

            TextInputFieldModel {
                id: textInputFieldModel
            }

            Component.onCompleted: {
                textInputFieldModel.init()
            }

            Keys.onShortcutOverride: function(event) {
                if (readOnly) {
                    return
                }

                if (event.key === Qt.Key_Enter || event.key === Qt.Key_Return
                        || event.key === Qt.Key_Escape) {
                    event.accepted = true
                    return
                }

                if (textInputFieldModel.isShortcutAllowedOverride(event.key, event.modifiers)) {
                    event.accepted = true
                } else {
                    event.accepted = false

                    root.focus = false
                    root.textEditingFinished(valueInput.text)
                }
            }

            Keys.onPressed: function(event) {
                var isAcceptKey = event.key === Qt.Key_Enter || event.key === Qt.Key_Return
                var isEscapeKey = event.key === Qt.Key_Escape
                if (isAcceptKey || isEscapeKey) {
                    root.focus = false
                    root.textEditingFinished(valueInput.text)
                }

                if (isAcceptKey) {
                    root.accepted()
                }

                if (isEscapeKey) {
                    root.escapted()
                }
            }

            onActiveFocusChanged: {
                if (activeFocus) {
                    navCtrl.requestActive()
                    selectAll()
                } else {
                    deselect()
                    root.textEditingFinished(valueInput.text)
                }
            }

            onTextChanged: {
                if (!acceptableInput) {
                    return
                }

                root.textChanged(text)
            }

            onTextEdited: {
                if (!acceptableInput) {
                    return
                }

                root.textEdited(text)
            }
        }

        StyledTextLabel {
            id: measureUnitsLabel

            Layout.alignment: Qt.AlignVCenter

            color: ui.theme.fontPrimaryColor
            visible: !root.isIndeterminate && !isEmpty
        }

        FlatButton {
            id: clearTextButtonItem

            Layout.fillHeight: true
            Layout.preferredWidth: height
            Layout.margins: root.accessoriesPadding

            toolTipTitle: qsTrc("global", "Clear")
            icon: IconCode.CLOSE_X_ROUNDED
            visible: root.clearTextButtonVisible

            transparent: true
            accentButton: true

            navigation.panel: navCtrl.panel
            navigation.order: navCtrl.order + 1

            onClicked: {
                root.clear()
                navCtrl.requestActive()
            }
        }

        Item {
            Layout.fillWidth: measureUnitsLabel.visible
        }
    }

    StyledTextLabel {
        id: undefinedValueLabel

        anchors.verticalCenter: parent.verticalCenter
        anchors.left: parent.left
        anchors.leftMargin: 12

        text: root.indeterminateText
        color: ui.theme.fontPrimaryColor
        visible: root.isIndeterminate && valueInput.activeFocus === false
    }

    states: [
        State {
            name: "HOVERED"
            when: root.containsMouse && !valueInput.activeFocus
            PropertyChanges { target: background; border.color: Utils.colorWithAlpha(ui.theme.accentColor, 0.6) }
        },

        State {
            name: "FOCUSED"
            when: valueInput.activeFocus
            PropertyChanges { target: background; border.color: ui.theme.accentColor }
        }
    ]

    MouseArea {
        id: clickableArea

        anchors.top: parent.top
        anchors.left: parent.left

        height: parent.height
        width: clearTextButtonItem.visible ? parent.width - clearTextButtonItem.width : parent.width

        propagateComposedEvents: true
        hoverEnabled: true
        cursorShape: root.readOnly ? Qt.ArrowCursor : Qt.IBeamCursor

        onPressed: function(mouse) {
            root.ensureActiveFocus()
            navCtrl.requestActiveByInteraction()
            mouse.accepted = false
        }
    }
}
